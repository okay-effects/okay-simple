package okay.effects.compat

import okay.effects.*
import okay.effects.machine.Machine

/** ONE APPLICATION OF TWO COMPILERS: Scala 3 code takes the effect, the handler and the program Scala 2.13 compiled,
 * as they are, beside its own — the types at the boundary the same */
class MixedSuite extends munit.FunSuite {
  val counting = new Counting(Machine)
  import counting.fx.*
  import counting.fx.std.*

  /** a Scala 3 program over Scala 2's effect: context bounds; the rows by the 2.13 build's names */
  def countedState[R[_]: {Has[Counter]#L, Has[(State % Int)#L]#L}]: Int ! R =
    counting.tick[R].andThen(counting.tick[R]).andThen(counting.peek[R]).flatMap(n => modify[Int, R](_ + n).as(n))

  type Row[X] = (*[Counter] + (Writer % String) + (State % Int))#L[X]

  test("Scala 2.13's program and handler, in Scala 3, beside Scala 3's program, in one row") {
    val both: Int ! Row = counting.greetAll[Row](List("Ann")).flatMap(n => countedState[Row].map(_ + n))
    assertEquals(run(state(100)(writer[String](counting.counting(both)))), (103, (List("hello, Ann"), (3, 4))))
  }

  test("Scala 2.13's row type is Scala 3's") {
    val p: Int ! (*[Counter] + (Writer % String))#L = counting.greetAll[counting.Row](List("Ann", "Bob"))
    assertEquals(run(writer[String](counting.counting(p))), (List("hello, Ann", "hello, Bob"), (2, 2)))
  }
}
