package okay.effects.defaults

import okay.effects._
import okay.effects.std._

/** the defaults: `import okay.effects._`, `import okay.effects.std._` — the machine, nothing else to do */
class DefaultsSuite extends munit.FunSuite {
  type Row[X] =
    Sum[({ type L[x] = Sum[Void, ({ type M[y] = Reader[String, y] })#M, x] })#L, ({ type M[y] = State[Int, y] })#M, X]

  def greet[R[_]](implicit r: In[({ type L[x] = Reader[String, x] })#L, R], s: In[({ type L[x] = State[Int, x] })#L, R])
    : String ! R =
    ask[String, R].flatMap(name => modify[Int, R](_ + 1).map(_ => s"hello, $name"))

  test("perform, handle, run: the standard effects, on the machine") {
    assertEquals(run(state(0)(reader("Ann")(greet[Row]))), (1, "hello, Ann"))
  }
  test("an effect of one's own: perform and handle") {
    assertEquals(run(handle[Ticket, Ticket, Pure, Int, Int](perform[Ticket].apply[Ticket, Int](Ticket.Next(n => n)),
          new TailHandler[Ticket, Pure, Int, Int] {
            def ret(a: Int) = pure[Int, Pure](a)
            def answer[X](op: Ticket[X]): X = op match { case Ticket.Next(answer) => answer(7) }
          })), 7)
  }
  test("tail calls, a million") {
    def even(n: Int): Boolean ! Pure = if (n == 0) pure[Boolean, Pure](true) else tailcall(odd(n - 1))
    def odd(n: Int): Boolean ! Pure = if (n == 0) pure[Boolean, Pure](false) else tailcall(even(n - 1))
    assert(run(even(1000000)))
  }
  test("delimited control: import okay.effects.delimited._") {
    val d = okay.effects.delimited
    val f = d.reset[String, String => String ! Pure, Pure](
      d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((name: String) => k(name))))(
        "hello, " + _))
    assertEquals(run(f.flatMap(g => g("Ann"))), "hello, Ann")
  }
}

sealed trait Ticket[X]
object Ticket { final case class Next[X](answer: Int => X) extends Ticket[X] }
