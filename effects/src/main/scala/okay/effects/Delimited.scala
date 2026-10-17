package okay.effects

/**
 * DELIMITED CONTROL WITH ANSWER-TYPE MODIFICATION, over an implementation `fx`: `Cps[A, B, C, R]` a program of value
 * `A` whose `reset`'s answer, `B` where it is, is `C` outside — `shift` may change it (Danvy and Filinski). Its own
 * type: `A ! R` has no index for an answer type. An implementation offers it as its `delimited`; by default
 * `Delimited.ByCps`, continuation-passing over `!`, on any implementation; one with continuations of its own that
 * change answer types offers its own. `shift`/`reset` with no change of the answer type are `Std`'s, an effect.
 */
trait Delimited[F <: Effects] {
  val fx: F
  import fx._

  /** a program of value `A`, its `reset`'s answer `B` changed to `C`, over the row `R` */
  type Cps[A, B, C, R[_]]

  def pure[A, B, R[_]](a: A): Cps[A, B, B, R]
  def map[A, A2, B, C, R[_]](p: Cps[A, B, C, R])(f: A => A2): Cps[A2, B, C, R]
  def flatMap[A, A2, B, C, D, R[_]](p: Cps[A, B, C, R])(f: A => Cps[A2, D, B, R]): Cps[A2, D, C, R]
  /** a program of `R`'s effects, the answer type as it is */
  def lift[A, B, R[_]](p: A ! R): Cps[A, B, B, R]
  /** the rest up to the nearest `reset` taken as `k`, of the answer `B`, any number of times; `f`'s value, of `C`,
   * the `reset`'s */
  def shift[A, B, C, R[_]](f: (A => B ! R) => C ! R): Cps[A, B, C, R]
  /** the delimiter: a program whose answer is its value's type, `A`, changed to `C` */
  def reset[A, C, R[_]](p: Cps[A, A, C, R]): C ! R

  implicit final class CpsOps[A, B, C, R[_]](p: Cps[A, B, C, R]) {
    def map[A2](f: A => A2): Cps[A2, B, C, R] = Delimited.this.map(p)(f)
    def flatMap[A2, D](f: A => Cps[A2, D, B, R]): Cps[A2, D, C, R] = Delimited.this.flatMap(p)(f)
  }
}

object Delimited {
  /** continuation-passing over the implementation's programs: `Cps[A, B, C, R]` a function from the rest, of the
   * answer `B`, to the `reset`'s program of `C`. Each step `delay`ed — the call into the program before it, and into
   * the rest — so the implementation's run, a loop, keeps binds nested either way in constant stack */
  class ByCps[F <: Effects](val fx: F) extends Delimited[F] {
    import fx._

    type Cps[A, B, C, R[_]] = (A => B ! R) => C ! R

    def pure[A, B, R[_]](a: A): Cps[A, B, B, R] = k => delay(k(a))
    def map[A, A2, B, C, R[_]](p: Cps[A, B, C, R])(f: A => A2): Cps[A2, B, C, R] = k => delay(p(a => delay(k(f(a)))))
    def flatMap[A, A2, B, C, D, R[_]](p: Cps[A, B, C, R])(f: A => Cps[A2, D, B, R]): Cps[A2, D, C, R] =
      k => delay(p(a => delay(f(a)(k))))
    def lift[A, B, R[_]](p: A ! R): Cps[A, B, B, R] = k => fx.flatMap[A, B, R](p)(k)
    def shift[A, B, C, R[_]](f: (A => B ! R) => C ! R): Cps[A, B, C, R] = f
    def reset[A, C, R[_]](p: Cps[A, A, C, R]): C ! R = delay(p(a => fx.pure[A, R](a)))
  }
}
