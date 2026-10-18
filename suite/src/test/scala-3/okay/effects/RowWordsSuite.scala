package okay.effects

import okay.effects.std.*

/** the words of one row: the row named once, the program with no types in it */
class RowWordsSuite extends munit.FunSuite:
  case class Config(greeting: String, limit: Int)
  type Service[X] = (Reader % Config + Writer % String + State % Int)[X]

  object service extends std.On[Service]:
    def serve(user: String): String ! Service =
      for
        c <- ask[Config]
        n <- get
        reply <- if n >= c.limit then tell(s"refused $user: busy").as("try later")
                 else put(n + 1).andThen(tell(s"served $user")).as(s"${c.greeting} $user")
      yield reply

    /** the state's type from the row: one State in it */
    def counted: Int ! Service = get.flatMap(n => put(n + 1).as(n))

  val day = for a <- service.serve("ann"); b <- service.serve("bob"); c <- service.serve("cid") yield List(a, b, c)

  test("a service: config, log, counter — no types in its logic"):
    assertEquals(run(state(0)(writer[String](reader(Config("hello", 2))(day)))),
      (2, (List("served ann", "served bob", "refused cid: busy"), List("hello ann", "hello bob", "try later"))))

  test("get with no type: the state's from the row"):
    assertEquals(run(state(41)(writer[String](reader(Config("x", 1))(service.counted)))), (42, (Nil, 41)))

  test("words imported from a val"):
    val words = std.on[Service]
    import words.*
    val p: Int ! Service = ask[Config].map(_.limit)
    assertEquals(run(state(0)(writer[String](reader(Config("x", 7))(p))))._2._2, 7)

  type Failing[X] = (Error % String + State % Int)[X]
  object failing extends std.On[Failing]:
    def half(n: Int): Int ! Failing = if n % 2 == 0 then pure(n / 2) else raise(s"$n is odd")

  test("raise is a program of any value"):
    assertEquals(run(attempt[String](state(0)(failing.half(7)))), Left("7 is odd"))
    assertEquals(run(attempt[String](state(0)(failing.half(8)))), Right((0, 4)))
