package okay.simple.kernel

import okay.kernel.{Answers, Effects => KEffects, Fn, General, Key, Op, Prog, Program, Step, Thunk, Translating, Type}
import okay.simple.{Ops, Simple}
import scala.annotation.unchecked.uncheckedVariance

/**
 * okay-simple's interface ON OKAY-KERNEL'S PROTOCOL: on any implementation of it — okay-kernel's reference machine,
 * okay-java, okay-core — given its `Effects`. A program is the implementation's own `Prog`.
 *
 * The protocol finds a handler by an operation's KEY, the nearest of it; okay-simple by the operation's PLACE in its
 * row. The bridge keeps okay-simple's meaning exactly: every operation of okay-simple's is of ONE key, carrying its
 * place; each handler of okay-simple's is a handler of that key, answering place 0 and passing any other on, one
 * place in — so two effects of one type in a row are two places, as they are to okay-simple. A general handler is
 * the protocol's `General`; one answering in place its `Translating` (place 0 its answer, any other performed on, no
 * `k`); a stateful one the interface's default, a general handler of functions of the state; a remapping (`split`,
 * `embed`) a `Translating` renaming every place. The price of the exactness: an operation is performed again at
 * each handler of okay-simple's it crosses.
 */
final class OnKernel private (e: KEffects) extends Simple {
  import OnKernel._

  type ![+A, R] = Prog[A @uncheckedVariance]
  type Pure = Nothing
  type +[R, E] = Nothing
  type Split[E, R, Rest] = Place
  type In[E, R] = Place
  type Sub[R, S] = Vector[Int]

  implicit def splitOne[E]: Place = places(0)
  implicit def splitLast[R, E]: Place = places(0)
  implicit def splitInit[E, R, G, Rest](implicit s: Place): Place = placeAt(s.at + 1)
  implicit def in[E, R, Rest](implicit s: Place): Place = s
  implicit def subPure[S]: Vector[Int] = Vector()
  implicit def subOne[E, S](implicit p: Place): Vector[Int] = Vector(p.at)
  implicit def subMore[R, E, S](implicit r: Vector[Int], p: Place): Vector[Int] = p.at +: r

  private def prog[A](p: Prog[A]): Prog[A] = p
  /** THE TRUSTED STEPS, the only casts of the bridge: an operation of okay-simple's key is a `Placed` (`placed`); at
   * place 0, of its handler's operations (`opOf`) */
  private def placed[X](op: Op[X]): Placed[X] = op.asInstanceOf[Placed[X]]
  private def opOf[F[_], X](op: Any): F[X] = op.asInstanceOf[F[X]]
  private def fn[A, B](f: A => B): Fn[A, B] = new Fn[A, B] { def apply(a: A): B = f(a) }
  private def program[A](p: => Prog[A]): Program[A] = new Program[A] { def in(e: KEffects): Prog[A] = p }

  def pure[A, R](a: A): Prog[A] = e.pure(a)
  def flatMap[A, B, R](p: Prog[A])(f: A => Prog[B]): Prog[B] =
    e.flatMap[A, B](prog(p), fn((a: A) => prog[B](f(a))))
  def delay[A, R](p: => Prog[A]): Prog[A] = e.delay[A](new Thunk[Prog[A]] { def get(): Prog[A] = prog(p) })
  private def performAt[X](at: Int, op: Any): Prog[X] = e.perform[X](new Placed[X](at, op))
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: Place): Prog[X] = performAt[X](in.at, op)

  /** every operation through it with its place renamed by `f` */
  private def remap[A](p: Prog[A], f: Int => Int): Prog[A] =
    e.handle[A, A](new Translating[A, A] {
      def key(): Key[Any] = theKey
      def translate[X](op: Op[X]): Program[X] = {
        val o = placed(op)
        program(performAt[X](f(o.at), o.op))
      }
    }, program(prog(p)))
  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = {
    val at = s.at
    if (at == 0) p else remap[A](p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = remap[A](p, s)

  def handle[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    e.handle[A, B](new General[A, B] {
      def key(): Key[Any] = theKey
      def ret(a: A, ke: KEffects): Prog[B] = prog(h.ret(a))
      def apply[X](op: Op[X], k: Fn[X, Prog[B]], ke: KEffects): Prog[B] = {
        val o = placed(op)
        if (o.at == 0) prog(h.op[X](opOf[F, X](o.op), (x: X) => k.apply(x)))
        else e.flatMap[X, B](performAt[X](o.at - 1, o.op), k)
      }
    }, program(prog(p)))

  override def handleTail[E, F[_], R, A, B](p: Prog[A])(h: TailHandler[F, R, A, B])(
      implicit o: Ops.Aux[E, F]): Prog[B] =
    flatMap[A, B, R](e.handle[A, A](new Translating[A, A] {
      def key(): Key[Any] = theKey
      def translate[X](op: Op[X]): Program[X] = {
        val o = placed(op)
        if (o.at == 0) program(e.pure(h.answer[X](opOf[F, X](o.op))))
        else program(performAt[X](o.at - 1, o.op))
      }
    }, program(prog(p))))(a => h.ret(a))

  def run[A](p: Prog[A]): A =
    e.computation[A](program(prog(p)), java.util.Collections.emptySet[Type]()).start(Answers.NONE) match {
      case d: Step.Done[A @unchecked] => d.value
      case f: Step.Failed[A @unchecked] => throw f.error
      case y: Step.Yield[_, A @unchecked] => throw new IllegalStateException(s"an operation no handler took: ${y.op}")
      case _ => throw new IllegalStateException("cancelled")
    }
}

object OnKernel {
  final case class Place(at: Int)
  /** the places, one per depth, shared: no allocation where an operation is performed */
  private val places: Array[Place] = Array.tabulate(64)(Place(_))
  private def placeAt(i: Int): Place = if (i < places.length) places(i) else Place(i)

  /** the one key of okay-simple's operations */
  val theKey: Key[Any] = Key.of[Any](Type.of("okay.simple"))

  /** an operation of okay-simple's: its place in its row, and the operation itself */
  final class Placed[X](val at: Int, val op: Any) extends Op[X] {
    def key(): Key[Any] = theKey
    override def toString: String = s"$op at $at"
  }

  /** okay-simple's interface on the implementation `e` of the protocol, seen through the interface only */
  def on(e: KEffects): Simple = new OnKernel(e)
}
