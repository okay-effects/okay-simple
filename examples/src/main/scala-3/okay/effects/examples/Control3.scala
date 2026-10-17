package okay.effects.examples

import okay.effects.Runner

/**
 * CONTROL, SCALA 3, over any implementation `fx`: generators (`Yield`), a search (`Choose`'s `first`), errors recovered, delimited control —
 * `shift`/`reset` with the answer type fixed (an effect of `std`), and with the answer type changed (`delimited`).
 */
class Control3(using val fx: Runner):
  import fx.*
  import fx.std.*

  /** A GENERATOR: values given out one by one, the generator going on */
  def countdown[R[_]: Has[Yield % Int]](n: Int): Unit ! R =
    if n == 0 then pure(()) else emit[Int, R](n).andThen(countdown[R](n - 1))

  def collected: (List[Int], Unit) = run(collect[Int](countdown[Yield % Int](3)))
  /** the first two, the generator stopped at the third */
  def firstTwo: (List[Int], Option[Unit]) = run(take[Int](2)(countdown[Yield % Int](5)))
  /** each value to a consumer with effects of its own: a sum in a state */
  def summed: (Int, Unit) =
    run(state(0)(each[Int](countdown[State % Int + Yield % Int](4))(n => modify[Int, State % Int](_ + n))))

  /** A SEARCH: the choices tried in order, the first with no `fail` kept */
  def pythagorean(max: Int): Option[(Int, Int, Int)] =
    def triple[R[_]: Has[Choose]]: (Int, Int, Int) ! R =
      for
        a <- among[Int, R]((1 to max).toList)
        b <- among[Int, R]((a to max).toList)
        c <- among[Int, R]((b to max).toList)
        t <- if a * a + b * b == c * c then pure[(Int, Int, Int), R]((a, b, c)) else fail[(Int, Int, Int), R]
      yield t
    run(first(triple[Choose]))

  /** ERRORS: raised, recovered — the rest of the program dropped, the recovery in its place */
  def half[R[_]: Has[Error % String]](n: Int): Int ! R =
    if n % 2 == 0 then pure(n / 2) else raise[String, R, Int](s"$n is odd")
  def halves: (Int, Int) =
    (run(recover[String](half[Error % String](10))(_ => pure(-1))), run(recover[String](half[Error % String](7))(e => pure(e.length))))

  /** SHIFT AND RESET, the answer type fixed: `k` the rest up to the `reset`, resumed twice — 1 + (1 + 10) */
  type C = Control % (Int ! Pure)
  def twice: Int = run(reset[Int, Pure](shift[Int, Int, Pure, Pure + C](k => k(10).flatMap(k)).map(_ + 1)))

  /** THE ANSWER TYPE CHANGED (`delimited`): the body's answer a `String`, the `reset`'s a function of the name */
  def greeting: String =
    val d = fx.delimited
    val f = d.reset[String, String => String ! Pure, Pure](
      d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((name: String) => k(name))))("hello, " + _))
    run(f.flatMap(g => g("Ann")))
