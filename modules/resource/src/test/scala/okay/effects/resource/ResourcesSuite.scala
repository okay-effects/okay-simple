package okay.effects.resource

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ResourcesChecks(val fx0: Runner) extends munit.FunSuite {
  val r = Resources(fx0)
  import r._
  import r.fx._
  import r.fx.std._

  type Wr[X] = Writer[String, X]
  type Out[X] = Sum[Void, Wr, X]
  type Sc[X] = Scope[Unit ! Out, X]
  type Er[X] = Error[String, X]
  type In0[X] = Sum[({ type L[x] = Sum[({ type M[y] = Sum[Void, Wr, y] })#M, Sc, x] })#L, Er, X]

  def open(name: String): String ! In0 = tell[String, In0](s"open $name").as(name)
  def close(name: String): Unit ! Out = tell[String, Out](s"close $name")
  def use2(fail: Boolean): Int ! In0 =
    bracket[String, Int, Out, In0](open("a"))(close)(a =>
      bracket[String, Int, Out, In0](open("b"))(close)(b =>
        tell[String, In0](s"use $a $b").flatMap(_ => if (fail) raise[String, In0, Int]("boom") else pure[Int, In0](2))))

  test("released, the last acquired first, when the scope ends") {
    assertEquals(fx0.run(writer[String](scoped(attempt[String](use2(false))))),
      (List("open a", "open b", "use a b", "close b", "close a"), Right(2)))
  }
  test("released when an error ends the scope, handled inside it") {
    assertEquals(fx0.run(writer[String](scoped(attempt[String](use2(true))))),
      (List("open a", "open b", "use a b", "close b", "close a"), Left("boom")))
  }
}

class ResourcesOnMachine extends ResourcesChecks(Machine)
class ResourcesOnFree extends ResourcesChecks(Free)
class ResourcesOnTagless extends ResourcesChecks(okay.effects.tagless.Tagless.on(Machine))
