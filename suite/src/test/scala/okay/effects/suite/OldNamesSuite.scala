package okay.effects.suite

import okay.effects.machine.Machine._
import okay.effects.machine.Machine.std._

import okay.effects.Sum
import scala.annotation.nowarn

/** THE OLD NAMES, deprecated, still the same effects and handlers: `Error` and `Yield` are `Throws` and `Emit`; the
 * handlers in lower case are the effects' `run` and the rest. Shared source, 2.13 and 3 */
@nowarn("cat=deprecation")
class OldNamesSuite extends munit.FunSuite {
  type Er[X] = Error[String, X]
  type Y[X] = Yield[Int, X]
  type SI[X] = State[Int, X]
  type SY[X] = Sum[SI, Y, X]

  def nums[R[_]](n: Int)(implicit in: In[Y, R]): Unit ! R =
    if (n == 0) pure[Unit, R](()) else emit[Int, R](n).flatMap(_ => nums[R](n - 1))

  test("Error is Throws: raised, attempted, recovered; Error.Raise is Throws.Raise") {
    val p: Int ! Er = raise[String, Er, Int]("no")
    assertEquals(run(attempt[String](p)), Left("no"))
    assertEquals(run(Throws.run[String](p)), Left("no"))
    assertEquals(run(recover[String](p)((e: String) => pure[Int, Pure](e.length))), 2)
    assertEquals(run(Throws.recover[String]((e: String) => pure[Int, Pure](e.length))(p)), 2)
    val raised: Throws[String, Int] = Error.Raise[String, Int]("x")
    assertEquals(raised, Throws.Raise[String, Int]("x"))
  }

  test("Yield is Emit: collected, each, taken; Yield.Emit is Emit.Send") {
    assertEquals(run(collect[Int](nums[Y](2))), (List(2, 1), ()))
    assertEquals(run(take[Int](1)(nums[Y](2))), (List(2), None))
    assertEquals(run(State.run(0)(each[Int](nums[SY](3))(o => modify[Int, SI](_ + o)))), (6, ()))
    val sent: Yield[Int, Unit] = Yield.Emit[Int, Unit](3, (u: Unit) => u)
    val value = sent match { case Emit.Send(v, _) => v }
    assertEquals(value, 3)
  }

  test("state, reader, writer, all, first, freshFrom: the effects' handlers") {
    assertEquals(run(state(1)(get[Int, SI])), (1, 1))
    assertEquals(run(reader("a")(ask[String, ({ type L[x] = Reader[String, x] })#L])), "a")
    assertEquals(run(all(among[Int, Choose](List(1, 2)))), List(1, 2))
    assertEquals(run(first(among[Int, Choose](List(1, 2)))), Some(1))
    assertEquals(run(freshFrom(5)(fresh[Fresh])), 5L)
  }
}
