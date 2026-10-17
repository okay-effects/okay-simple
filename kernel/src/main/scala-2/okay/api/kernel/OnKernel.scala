package okay.api.kernel

import okay.kernel.{Effects => KEffects, Prog}
import okay.api.{Effect, Effects}
import okay.std.Std
import scala.annotation.unchecked.uncheckedVariance

/** okay-api's interface, Scala 2's, on okay-kernel's protocol (`Protocol`): its rows' places numbers */
class OnKernel private (e: KEffects) extends Effects {
  import Protocol._
  private val k = new Protocol(e)

  type ![+A, R] = Prog[A @uncheckedVariance]
  type Pure = Nothing
  type +[R, E <: Effect] = Nothing
  type Split[E <: Effect, R, Rest] = Place
  type In[E <: Effect, R] = Place
  type Sub[R, S] = Vector[Int]

  implicit def splitOne[E <: Effect]: Place = placeAt(0)
  implicit def splitLast[R, E <: Effect]: Place = placeAt(0)
  implicit def splitInit[E <: Effect, R, G <: Effect, Rest](implicit s: Place): Place = placeAt(s.at + 1)
  implicit def in[E <: Effect, R, Rest](implicit s: Place): Place = s
  implicit def subPure[S]: Vector[Int] = Vector()
  implicit def subOne[E <: Effect, S](implicit p: Place): Vector[Int] = Vector(p.at)
  implicit def subMore[R, E <: Effect, S](implicit r: Vector[Int], p: Place): Vector[Int] = p.at +: r

  /** THE TRUSTED STEP of the interface's side: an operation at place 0, of its handler's operations */
  private def opOf[E <: Effect, X](op: Any): Op[E, X] = op.asInstanceOf[Op[E, X]]

  def pure[A, R](a: A): Prog[A] = k.pure(a)
  def flatMap[A, B, R](p: Prog[A])(f: A => Prog[B]): Prog[B] = k.flatMap(p)(f)
  def delay[A, R](p: => Prog[A]): Prog[A] = k.delay(p)
  protected def performOp[E <: Effect, R, X](op: Op[E, X])(implicit in: Place): Prog[X] = k.performAt[X](in.at, op)

  def split[E <: Effect, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = k.toEnd(p, s.at)
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = k.remap[A](p, s)

  def handleLast[E <: Effect, R, A, B](p: Prog[A])(h: Handler[E, R, A, B]): Prog[B] =
    k.general[A, B](p, a => h.ret(a), new Clause[B] {
      def apply[X](op: Any, kx: X => Prog[B]): Prog[B] = h.op[X](opOf[E, X](op), kx)
    })
  override def handleTailLast[E <: Effect, R, A, B](p: Prog[A])(h: TailHandler[E, R, A, B]): Prog[B] =
    k.answering[A, B](p, a => h.ret(a), new Answer { def apply[X](op: Any): X = h.answer[X](opOf[E, X](op)) })

  def run[A](p: Prog[A]): A = k.run(p)
}

object OnKernel {
  /** okay-api's interface on the implementation `e` of the protocol, with the standard effects, seen through the
   * interface only */
  def on(e: KEffects): Std = new OnKernel(e) with Std
}
