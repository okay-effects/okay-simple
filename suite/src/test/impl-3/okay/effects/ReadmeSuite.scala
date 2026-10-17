package okay.effects

/** README's program as it is, Scala 3's: one import gives the types, the primitives, the effects and their handlers */
class ReadmeSuite extends munit.FunSuite:
  import okay.effects.simple.Machine.*

  def greet[R[_]: {Has[Reader % String], Has[Writer % String], Has[State % Int]}]: Int ! R =
    for
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + name.length)
    yield n

  type Row = Reader % String + Writer % String + State % Int

  test("README's program, one import"):
    assertEquals(run(reader("Ann")(state(10)(writer[String](greet[Row])))), (13, (List("hello, Ann"), 10)))

  test("an effect with no parameter, by its own name in a row"):
    def among3[R[_]: Has[Choose]]: Int ! R = among[Int, R](List(1, 2, 3))
    assertEquals(run(all(among3[Choose])), List(1, 2, 3))
