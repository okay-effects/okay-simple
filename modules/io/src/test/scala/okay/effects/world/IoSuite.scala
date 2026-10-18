package okay.effects.worldchecks.checks


import okay.effects.world.{Clock, Console, Env, Io}

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class IoChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val io = Io()
  import io._
  import io.fx._

  type App[X] = Sum[({ type L[x] = Sum[({ type M[y] = Sum[Void, Console, y] })#M, Clock, x] })#L, Env, X]

  /** a program naming what it touches: the console, the clock, the environment */
  def greet: Int ! App =
    getEnv[App]("GREETING").flatMap { g =>
      printLine[App]("name?").flatMap(_ => readLine[App]).flatMap { name =>
        now[App].flatMap(t0 => now[App].flatMap { t1 =>
          printLine[App](s"${g.getOrElse("hi")}, ${name.getOrElse("nobody")}").as((t1 - t0).toInt)
        })
      }
    }

  test("under pure handlers: the lines written, the time it took") {
    assertEquals(io.fx.run(console(List("Ann"))(clock(1000, 5)(env(Map("GREETING" -> "hello"))(greet)))),
      (List("name?", "hello, Ann"), 5))
  }
  test("past the inputs, no line; no variable, the default") {
    assertEquals(io.fx.run(console(Nil)(clock(0, 1)(env(Map.empty)(greet)))), (List("name?", "hi, nobody"), 1))
  }
  test("the world's clock and environment") {
    val t = io.fx.run(systemClock(now[Clock]))
    assert(t > 0)
    assertEquals(io.fx.run(systemEnv(getEnv[Env]("OKAY_NO_SUCH_VARIABLE"))), None)
  }
}

class IoOnMachine extends IoChecks(Machine)
class IoOnFree extends IoChecks(Free)
class IoOnTagless extends IoChecks(okay.effects.tagless.Tagless.on(Machine))
