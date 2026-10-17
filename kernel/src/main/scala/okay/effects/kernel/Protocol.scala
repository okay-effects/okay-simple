package okay.effects.kernel

import okay.kernel.{Answers, Effects => KEffects, Fn, General, Key, Op => KOp, Prog, Program, Step, Thunk, Translating, Type}

/**
 * okay-effects' interface ON OKAY-KERNEL'S PROTOCOL, on any implementation of it —
 * okay-kernel's reference machine, okay-java, okay-core — given its `Effects`. A program is the implementation's own
 * `Prog`. `OnKernel` is the interface over it.
 *
 * The protocol finds a handler by an operation's KEY, the nearest of it; okay-effects by the operation's PLACE in its
 * row. The bridge keeps okay-effects' meaning exactly: every operation of okay-effects' is of ONE key, carrying its
 * place; each handler of okay-effects' is a handler of that key, answering place 0 and passing any other on, one
 * place in — so two effects of one type in a row are two places, as they are to okay-effects. A general handler is
 * the protocol's `General`; one answering in place its `Translating` (place 0 its answer, any other performed on, no
 * `k`); a stateful one the interface's default, a general handler of functions of the state; a remapping (`split`,
 * `embed`) a `Translating` renaming every place. The price of the exactness: an operation is performed again at
 * each handler of okay-effects' it crosses.
 */
private[kernel] final class Protocol(e: KEffects) {
  import Protocol._

  /** THE TRUSTED STEP, the protocol's own: an operation of okay-effects' key is a `Placed` */
  private def placed[X](op: KOp[X]): Placed[X] = op.asInstanceOf[Placed[X]]
  private def fn[A, B](f: A => B): Fn[A, B] = new Fn[A, B] { def apply(a: A): B = f(a) }
  private def program[A](p: => Prog[A]): Program[A] = new Program[A] { def in(e: KEffects): Prog[A] = p }

  def pure[A](a: A): Prog[A] = e.pure(a)
  def flatMap[A, B](p: Prog[A])(f: A => Prog[B]): Prog[B] = e.flatMap[A, B](p, fn(f))
  def delay[A](p: => Prog[A]): Prog[A] = e.delay[A](new Thunk[Prog[A]] { def get(): Prog[A] = p })
  def performAt[X](at: Int, op: Any): Prog[X] = e.perform[X](new Placed[X](at, op))

  /** every operation through it with its place renamed by `f` */
  def remap[A](p: Prog[A], f: Int => Int): Prog[A] =
    e.handle[A, A](new Translating[A, A] {
      def key(): Key[Any] = theKey
      def translate[X](op: KOp[X]): Program[X] = {
        val o = placed(op)
        program(performAt[X](f(o.at), o.op))
      }
    }, program(p))
  /** the effect at `at` moved to place 0, the ones nearer one further in */
  def toEnd[A](p: Prog[A], at: Int): Prog[A] =
    if (at == 0) p else remap[A](p, i => if (i == at) 0 else if (i < at) i + 1 else i)

  /** place 0 handled by `op`'s clause, a general one: the operation as it was performed, its `k` */
  def general[A, B](p: Prog[A], onValue: A => Prog[B], op: Clause[B]): Prog[B] =
    e.handle[A, B](new General[A, B] {
      def key(): Key[Any] = theKey
      def ret(a: A, ke: KEffects): Prog[B] = onValue(a)
      def apply[X](o: KOp[X], k: Fn[X, Prog[B]], ke: KEffects): Prog[B] = {
        val pl = placed(o)
        if (pl.at == 0) op[X](pl.op, (x: X) => k.apply(x))
        else e.flatMap[X, B](performAt[X](pl.at - 1, pl.op), k)
      }
    }, program(p))

  /** place 0 answered in place by `answer` */
  def answering[A, B](p: Prog[A], ret: A => Prog[B], answer: Answer): Prog[B] =
    flatMap[A, B](e.handle[A, A](new Translating[A, A] {
      def key(): Key[Any] = theKey
      def translate[X](o: KOp[X]): Program[X] = {
        val pl = placed(o)
        if (pl.at == 0) program(e.pure(answer[X](pl.op)))
        else program(performAt[X](pl.at - 1, pl.op))
      }
    }, program(p)))(ret)

  def run[A](p: Prog[A]): A =
    e.computation[A](program(p), java.util.Collections.emptySet[Type]()).start(Answers.NONE) match {
      case d: Step.Done[A @unchecked] => d.value
      case f: Step.Failed[A @unchecked] => throw f.error
      case y: Step.Yield[_, A @unchecked] => throw new IllegalStateException(s"an operation no handler took: ${y.op}")
      case _ => throw new IllegalStateException("cancelled")
    }
}

private[kernel] object Protocol {
  final case class Place(at: Int)
  /** the places, one per depth, shared: no allocation where an operation is performed */
  private val places: Array[Place] = Array.tabulate(64)(Place(_))
  def placeAt(i: Int): Place = if (i < places.length) places(i) else Place(i)

  /** the one key of okay-effects' operations */
  val theKey: Key[Any] = Key.of[Any](Type.of("okay.effects"))

  /** an operation of okay-effects': its place in its row, and the operation itself */
  final class Placed[X](val at: Int, val op: Any) extends KOp[X] {
    def key(): Key[Any] = theKey
    override def toString: String = s"$op at $at"
  }

  /** a general handler's clause: the operation, of whatever effect, and its `k` */
  trait Clause[B] { def apply[X](op: Any, k: X => Prog[B]): Prog[B] }
  /** a clause answering in place */
  trait Answer { def apply[X](op: Any): X }
}
