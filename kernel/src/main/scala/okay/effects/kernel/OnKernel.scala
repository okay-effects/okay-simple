package okay.effects.kernel

import okay.kernel.{Effects => KEffects, Prog}
import okay.effects.Effects
import scala.annotation.unchecked.uncheckedVariance

/** okay-effects' interface on okay-kernel's protocol (`Protocol`), for both compilers: its rows' places numbers */
class OnKernel private (e: KEffects) extends Effects {
  import Protocol._
  private val k = new Protocol(e)

  type ![+A, R[_]] = Prog[A @uncheckedVariance]
  type Split[E[_], R[_], Rest[_]] = Place
  type In[E[_], R[_]] = Place
  type Sub[R[_], S[_]] = Vector[Int]

  implicit def splitOne[E[_]]: Place = placeAt(0)
  implicit def splitLast[R[_], E[_]]: Place = placeAt(0)
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Place): Place = placeAt(s.at + 1)
  implicit def subPure[S[_]]: Vector[Int] = Vector()
  implicit def subOne[E[_], S[_]](implicit p: Place): Vector[Int] = Vector(p.at)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Vector[Int], p: Place): Vector[Int] = p.at +: r

  /** THE TRUSTED STEP of the interface's side: an operation at place 0, of its handler's operations */
  private def opOf[E[_], X](op: Any): E[X] = op.asInstanceOf[E[X]]

  def pure[A, R[_]](a: A): Prog[A] = k.pure(a)
  def flatMap[A, B, R[_]](p: Prog[A])(f: A => Prog[B]): Prog[B] = k.flatMap(p)(f)
  def delay[A, R[_]](p: => Prog[A]): Prog[A] = k.delay(p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: Place): Prog[X] = k.performAt[X](in.at, op)

  def split[E[_], R[_], Rest[_], A](p: Prog[A])(implicit s: Place): Prog[A] = k.toEnd(p, s.at)
  def embed[R[_], S[_], A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = k.remap[A](p, s)

  def handleLast[E[_], R[_], A, B](p: Prog[A], h: Handler[E, R, A, B]): Prog[B] =
    k.general[A, B](p, a => h.ret(a), new Clause[B] {
      def apply[X](op: Any, kx: X => Prog[B]): Prog[B] = h.op[X](opOf[E, X](op), kx)
    })
  override def handleLast[E[_], R[_], A, B](p: Prog[A], h: TailHandler[E, R, A, B]): Prog[B] =
    k.answering[A, B](p, a => h.ret(a), new Answer { def apply[X](op: Any): X = h.answer[X](opOf[E, X](op)) })

  def run[A](p: Prog[A]): A = k.run(p)
}

object OnKernel {
  /** okay-effects' interface on the implementation `e` of the protocol, seen through the interface only; its standard effects its `std` */
  def on(e: KEffects): Effects = new OnKernel(e)
}
