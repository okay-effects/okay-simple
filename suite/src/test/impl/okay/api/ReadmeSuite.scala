package okay.api

/** README's program as it is: one import gives the types, the primitives, the effects and their handlers */
class ReadmeSuite extends munit.FunSuite {
  import okay.simple.machine.Simple._

  def greet[R](implicit r: In[Reader % String, R], w: In[Writer % String, R], s: In[State % Int, R]): Int ! R =
    for {
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + name.length)
    } yield n

  type Row = (Reader % String) + (Writer % String) + (State % Int)

  test("README's program, one import") {
    assertEquals(run(reader("Ann")(state(10)(writer[String](greet[Row])))), (13, (List("hello, Ann"), 10)))
  }

  test("an effect of one's own, its name *[F], from the same import") {
    def among3[R](implicit in: In[*[Choose], R]): Int ! R = among[Int, R](List(1, 2, 3))
    assertEquals(run(all(among3[*[Choose]])), List(1, 2, 3))
  }
}
