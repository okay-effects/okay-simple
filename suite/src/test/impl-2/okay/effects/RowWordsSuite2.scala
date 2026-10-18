package okay.effects

/** the words of one row, Scala 2's: the row named once, by names */
class RowWordsSuite2 extends munit.FunSuite {
  import okay.effects.machine.Machine._
  import okay.effects.machine.Machine.std._

  case class Config(greeting: String, limit: Int)
  type Service[X] = ((Reader % Config) + (Writer % String) + (State % Int))#L[X]

  object service extends okay.effects.machine.Machine.std.On[Service] {
    def serve(user: String): String ! Service =
      for {
        c <- ask[Config]
        n <- get[Int]
        reply <- if (n >= c.limit) tell(s"refused $user: busy").as("try later")
                 else put(n + 1).andThen(tell(s"served $user")).as(s"${c.greeting} $user")
      } yield reply
  }

  test("a service in Scala 2: no row in its logic") {
    val day = for { a <- service.serve("ann"); b <- service.serve("bob"); c <- service.serve("cid") } yield List(a, b, c)
    assertEquals(run(state(0)(writer[String](reader(Config("hello", 2))(day)))),
      (2, (List("served ann", "served bob", "refused cid: busy"), List("hello ann", "hello bob", "try later"))))
  }

  object counter extends okay.effects.machine.Machine.std.On[((Reader % Int) + (State % Int))#L] {
    def tick: Int ! Row =
      for {
        step <- ask[Int]
        n    <- get[Int]
        _    <- put(n + step)
      } yield n + step
  }

  test("Row in Scala 2") {
    assertEquals(run(state(0)(reader(5)(counter.tick.flatMap(_ => counter.tick)))), (10, 10))
  }
}
