package okay.effects.resilience

import okay.effects.Effects

/**
 * RETRIES over any implementation `fx`: a program that may raise `E` run again — `retry[E](times)(p)`, at most `times`
 * more times; `retryIf[E](times)(worth)(p)`, only while what was raised is `worth` it; `fallback[E](p)(q)`, `q` where
 * `p` raised. Each run of `p` starts anew: what it did in effects outside (told, written) stays done. A timeout needs
 * a runtime of its own: interop's ZIO or cats-effect, its program lifted.
 */
class Resilience[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation
   * to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  def retry[E](times: Int): RetryHandler[E] = new RetryHandler[E](times, _ => true)
  def retryIf[E](times: Int)(worth: E => Boolean): RetryHandler[E] = new RetryHandler[E](times, worth)
  final class RetryHandler[E](times: Int, worth: E => Boolean) {
    def apply[R[_], Rest[_], A](p: => A ! R)(
      implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): Either[E, A] ! Rest =
      attempt[E](p).flatMap {
        case Left(e) if times > 0 && worth(e) => new RetryHandler[E](times - 1, worth)[R, Rest, A](p)
        case done => pure[Either[E, A], Rest](done)
      }
  }

  def fallback[E]: FallbackHandler[E] = new FallbackHandler[E]
  final class FallbackHandler[E] {
    def apply[R[_], Rest[_], A](p: A ! R)(q: E => A ! Rest)(
      implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): A ! Rest =
      recover[E](p)(q)
  }
}

object Resilience {
  def apply()(implicit fx: Effects): Resilience[fx.type] = {
    implicit val exact: fx.type = fx
    new Resilience[fx.type]
  }
}
