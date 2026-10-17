package okay.effects.resilience

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ResilienceChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val rs = Resilience()
  import rs._
  import rs.fx._
  import rs.fx.std._

  type St[X] = State[Int, X]
  type Er[X] = Error[String, X]
  type S1[X] = Sum[Void, St, X]
  type T[X] = Sum[S1, Er, X]

  /** fails until it has been tried `n` times: the tries counted in a state outside */
  def flaky(n: Int): String ! T =
    modify[Int, T](_ + 1).flatMap(_ => get[Int, T]).flatMap(tries => if (tries < n) raise[String, T, String](s"try $tries") else pure[String, T]("ok"))

  test("retried until it succeeds") {
    assertEquals(rs.fx.run(state(0)(retry[String](5)(flaky(3)))), (3, Right("ok")))
  }
  test("given up after the retries: the last error") {
    assertEquals(rs.fx.run(state(0)(retry[String](1)(flaky(3)))), (2, Left("try 2")))
  }
  test("retried only while worth it") {
    assertEquals(rs.fx.run(state(0)(retryIf[String](5)(_ != "try 1")(flaky(3)))), (1, Left("try 1")))
  }
  test("a fallback where it raised") {
    assertEquals(rs.fx.run(state(0)(fallback[String](flaky(9))(e => pure[String, S1](s"fallback after $e")))), (1, "fallback after try 1"))
  }
}

class ResilienceOnMachine extends ResilienceChecks(Machine)
class ResilienceOnFree extends ResilienceChecks(Free)
class ResilienceOnTagless extends ResilienceChecks(okay.effects.tagless.Tagless.on(Machine))
