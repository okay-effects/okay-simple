package okay.effects.suite

import okay.effects.Std
import okay.effects.machine.Machine._
import okay.effects.machine.Machine.std._

/** ONE SPELLING OF A ROW FOR BOTH COMPILERS: `Row[name, X]` — the name of `*`, `%` and `+`, a row by its `L` in Scala
 * 2,
 * a row already in Scala 3. This file is shared source: it compiles and runs on 2.13 and on 3 as it is */
class RowSuite extends munit.FunSuite {
  type App[X] = Row[(Reader % String) + (State % Int), X]
  type Pick[X] = Row[*[Pure] + *[Choose], X]
  // one effect by its name too, where evidence is asked: no `#L` (Scala 2's) and no bare lambda (Scala 3's)
  type Reads[X] = Row[Reader % String, X]
  type Counts[X] = Row[State % Int, X]
  def greet[R[_]](implicit r: In[Reads, R], s: In[Counts, R]): String ! R =
    ask[String, R].flatMap(name => get[Int, R].flatMap(n => put[Int, R](n + 1).map(_ => s"$n: $name")))
  test("a row of parameterised effects by names, run") {
    assertEquals(run(state(10)(reader("Ann")(greet[App]))), (11, "10: Ann"))
  }
  test("a row of one effect after the empty one, by names") {
    val pairs: List[(Int, Int)] ! Pure =
      all(among[Int, Pick](List(1, 2)).flatMap(a => among[Int, Pick](List(10, 20)).map(b => (a, b))))
    assertEquals(run(pairs), List((1, 10), (1, 20), (2, 10), (2, 20)))
  }
}
