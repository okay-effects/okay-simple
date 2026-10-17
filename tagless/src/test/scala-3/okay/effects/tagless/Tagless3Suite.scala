package okay.effects.tagless

import okay.effects.Effects
import okay.effects.machine.Machine
import okay.effects.free.Free
import Tagless.*
import Tagless.std.*

/** Scala 3's words: rows by type lambdas, context bounds, for — and no implementation named until the run */
object Greeting:
  def greet[R[_]: {Has[Reader % String], Has[Writer % String], Has[State % Int]}]: Int ! R =
    for
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + 1)
    yield n

  type App = Reader % String + Writer % String + State % Int
  val greeted: (Int, (List[String], Int)) ! Pure = state(0)(writer[String](reader("Ann")(greet[App])))

class Tagless3Suite extends munit.FunSuite:
  test("on the machine and on Free, the implementation given") {
    locally { given Effects = Machine; assertEquals(run(Greeting.greeted), (1, (List("hello, Ann"), 0))) }
    locally { given Effects = Free; assertEquals(run(Greeting.greeted), (1, (List("hello, Ann"), 0))) }
  }
