package okay.api.compat

import okay.api.*
import okay.simple.*
import okay.simple.machine.Simple

/** ONE APPLICATION OF TWO COMPILERS: Scala 3 code takes the effect, the handler and the program Scala 2.13 compiled,
 * as they are, beside its own — the types at the boundary the same */
class MixedSuite extends munit.FunSuite {
  val counting = new Counting(Simple.effects)
  import counting.fx.*
  import counting.std.*

  /** a Scala 3 program over Scala 2's effect: context bounds, no parentheses */
  def countedState[R: {Has[*[Counter]]#L, Has[State % Int]#L}]: Int ! R =
    counting.tick[R].andThen(counting.tick[R]).andThen(counting.peek[R]).flatMap(n => modify[Int, R](_ + n).as(n))

  type Row = *[Counter] + Writer % String + State % Int

  test("Scala 2.13's program and handler, in Scala 3, beside Scala 3's program, in one row") {
    val both: Int ! Row = counting.greetAll[Row](List("Ann")).flatMap(n => countedState[Row].map(_ + n))
    assertEquals(run(state(100)(writer[String](counting.counting(both)))), (103, (List("hello, Ann"), (3, 4))))
  }

  test("Scala 2.13's row type is Scala 3's") {
    val p: Int ! (*[Counter] + Writer % String) = counting.greetAll[counting.Row](List("Ann", "Bob"))
    assertEquals(run(writer[String](counting.counting(p))), (List("hello, Ann", "hello, Bob"), (2, 2)))
  }
}
