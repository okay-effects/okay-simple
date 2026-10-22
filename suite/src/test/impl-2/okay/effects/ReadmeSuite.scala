package okay.effects

/** README's program as it is, Scala 2's: one import gives the types, the primitives, the effects and their handlers */
class ReadmeSuite extends munit.FunSuite {
  import okay.effects.machine.Machine._
  import okay.effects.machine.Machine.std._

  def greet[R[_]](implicit r: In[(Reader % String)#L, R], w: In[(Writer % String)#L, R], s: In[(State % Int)#L, R])
    : Int ! R =
    for {
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + name.length)
    } yield n

  type Row[X] = ((Reader % String) + (Writer % String) + (State % Int))#L[X]

  test("README's program, one import") {
    assertEquals(run(reader("Ann")(state(10)(writer[String](greet[Row])))), (13, (List("hello, Ann"), 10)))
  }

  test("an effect of no parameter: itself in a row of one, `*[F]` among names") {
    def among3[R[_]](implicit in: In[Choose, R]): Int ! R = among[Int, R](List(1, 2, 3))
    assertEquals(run(all(among3[Choose])), List(1, 2, 3))
    type C[X] = (*[Choose] + (State % Int))#L[X]
    assertEquals(run(state(0)(all(among3[C]))), (0, List(1, 2, 3)))
  }
}
