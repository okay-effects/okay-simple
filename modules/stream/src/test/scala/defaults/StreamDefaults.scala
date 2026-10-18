package defaults

import okay.effects._
import okay.effects.std._
import okay.effects.stream._

/** the defaults as a user writes them: imports, nothing else */
class StreamDefaults extends munit.FunSuite {
  type St[X] = State[Int, X]
  type Y[X] = Yield[Int, X]
  test("import okay.effects._, okay.effects.std._, okay.effects.stream._") {
    assertEquals(run(range[Pure](0, 5).map(_ * 2).toList), List(0, 2, 4, 6, 8))
    val totals = range[St](1, 4).evalMap(n => modify[Int, St](_ + n).flatMap(_ => get[Int, St])).toList
    assertEquals(run(state(0)(totals)), (6, List(1, 3, 6)))
    def naturals(n: Int): Unit ! Y = emit[Int, Y](n).flatMap(_ => naturals(n + 1))
    assertEquals(run(fromGenerator[Int](naturals(0)).take(3).toList), List(0, 1, 2))
    assertEquals(run(merge[Int, Pure](List(emits[Int, Pure](List(1, 3)), emits[Int, Pure](List(2, 4)))).toList), List(1, 2, 3, 4))
  }
}
