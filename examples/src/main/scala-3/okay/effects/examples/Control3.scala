package okay.effects.examples

import okay.effects.Runner

/**
 * CONTROL, SCALA 3, over any implementation `fx`: generators (`Emit`), a search (`Choose.first`), errors
 * recovered, delimited control —
 * `shift`/`reset` with the answer type fixed (an effect of `std`), and with the answer type changed (`delimited`).
 */
class Control3(using val fx: Runner):
  import fx.*
  import fx.std.*

  /** A GENERATOR: values given out one by one, the generator going on */
  def countdown[R[_]: Has[Emit % Int]](n: Int): Unit ! R =
    if n == 0 then pure(()) else emit[Int, R](n).andThen(countdown[R](n - 1))

  def collected: (List[Int], Unit) = run(Emit.collect[Int](countdown[Emit % Int](3)))
  /** the first two, the generator stopped at the third */
  def firstTwo: (List[Int], Option[Unit]) = run(Emit.take[Int](2)(countdown[Emit % Int](5)))
  /** each value to a consumer with effects of its own: a sum in a state */
  def summed: (Int, Unit) =
    run(State.run(0)(Emit.each[Int](n => modify[Int, State % Int](_ + n))(countdown[State % Int + Emit % Int](4))))

  /** A SEARCH: the choices tried in order, the first with no `fail` kept */
  def pythagorean(max: Int): Option[(Int, Int, Int)] =
    def triple[R[_]: Has[Choose]]: (Int, Int, Int) ! R =
      for
        a <- among[Int, R]((1 to max).toList)
        b <- among[Int, R]((a to max).toList)
        c <- among[Int, R]((b to max).toList)
        t <- if a * a + b * b == c * c then pure[(Int, Int, Int), R]((a, b, c)) else fail[(Int, Int, Int), R]
      yield t
    run(Choose.first(triple[Choose]))

  /** ERRORS: raised, recovered — the rest of the program dropped, the recovery in its place */
  def half[R[_]: Has[Throws % String]](n: Int): Int ! R =
    if n % 2 == 0 then pure(n / 2) else raise[String, R, Int](s"$n is odd")
  def halves: (Int, Int) =
    (run(Throws.recover[String](_ => pure(-1))(half[Throws % String](10))),
      run(Throws.recover[String](e => pure(e.length))(half[Throws % String](7))))

  /** SHIFT AND RESET, the answer type fixed: `k` the rest up to the `reset`, resumed twice — 1 + (1 + 10) */
  type C[X] = (Control % (Int ! Pure))[X]
  def twice: Int = run(reset[Int, Pure](shift[Int, Int, Pure, Pure + C](k => k(10).flatMap(k)).map(_ + 1)))

  /** THE ANSWER TYPE CHANGED (`delimited`): the body's answer a `String`, the `reset`'s a function of the name */
  def greeting: String =
    val d = fx.delimited
    val f = d.reset[String, String => String ! Pure, Pure](
      d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((name: String) => k(name))))(
        "hello, " + _))
    run(f.flatMap(g => g("Ann")))
