package okay.effects.tofu

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.cats.Cats
import okay.effects.machine.Machine
import okay.effects.free.Free

/** tofu's typeclasses over the effects, on an implementation */
abstract class TofuChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val t = Tofu[runner.type](Cats())
  import t._
  import t.cats.fx._
  import t.cats.fx.std._

  type RS[X] = Std.Reader[String, X]
  type Er[X] = Std.Error[String, X]
  type Row[X] = Sum[({ type L[x] = Sum[Void, RS, x] })#L, Er, X]
  type NoReader[X] = Sum[Void, Er, X]
  type NoError[X] = Sum[Void, RS, X]

  test("WithLocal: the context asked, and changed for a part") {
    val wl = withLocal[Row, NoReader, String]
    val p: String ! Row = wl.context.flatMap(c => wl.local(ask[String, Row].map(_ + "!"))(_.toUpperCase).map(c + " " + _))
    assertEquals(run(attempt[String](reader("ann")(p))), Right("ann ANN!"))
  }
  test("Errors: raised, handled with, restored") {
    val er = errors[Row, NoError, String]
    val handled: Int ! Row = er.tryHandleWith(er.raise[Int]("bad"))(e => Some(pure[Int, Row](e.length)))
    val unhandled: Int ! Row = er.tryHandleWith(er.raise[Int]("bad"))(_ => None)
    val restored: Option[Int] ! Row = er.restore(er.raise[Int]("bad"))
    assertEquals(run(attempt[String](reader("x")(handled))), Right(3))
    assertEquals(run(attempt[String](reader("x")(unhandled))), Left("bad"))
    assertEquals(run(attempt[String](reader("x")(restored))), Right(None))
  }
}

class TofuOnMachine extends TofuChecks(Machine)
class TofuOnFree extends TofuChecks(Free)
