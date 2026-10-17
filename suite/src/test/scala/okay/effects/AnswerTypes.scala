package okay.effects

/** delimited control changing answer types, one source for both compilers */
class AnswerTypes(val fx: Effects) {
  import fx._
  val d = fx.delimited

  /** the answer type changed: the body's a `String`, the `reset`'s a function of the name */
  def greeting: String = {
    val f: (String => String ! Pure) ! Pure = d.reset[String, String => String ! Pure, Pure](
      d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((s: String) => k(s))))(s => "hello, " + s))
    run(f.flatMap(g => g("Ann")))
  }

  /** the answer type as it is: `k` resumed twice, 1 + (1 + 10) */
  def twice: Int =
    run(d.reset[Int, Int, Pure](d.map(d.shift[Int, Int, Int, Pure](k => k(10).flatMap(k)))(_ + 1)))

  /** a program of effects under the `reset`, lifted: `ask` answered by the handler outside */
  type RS[X] = fx.std.Reader[String, X]
  type Row[X] = Sum[Void, RS, X]
  def lifted: Int = run(fx.std.reader("Ann")(d.reset[Int, Int, Row](
    d.flatMap(d.lift[String, Int, Row](fx.std.ask[String, Row]))(s => d.pure[Int, Int, Row](s.length)))))

  /** in constant stack: 100 000 binds */
  def binds(n: Int): Int =
    run(d.reset[Int, Int, Pure]((1 to n).foldLeft(d.pure[Int, Int, Pure](0))((p, _) => d.map(p)(_ + 1))))
}
