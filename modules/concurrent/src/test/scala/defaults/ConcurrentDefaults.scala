package defaults

import okay.effects._
import okay.effects.std._
import okay.effects.concurrent._

class ConcurrentDefaults extends munit.FunSuite {
  type Wr[X] = Writer[String, X]
  type Ch[X] = Coop[Int, X]
  type T[X] = Sum[({ type L[x] = Sum[Void, Wr, x] })#L, Ch, X]
  def worker(name: String, n: Int): Int ! T =
    if (n == 0) pure[Int, T](0) else tell[String, T](s"$name$n").flatMap(_ => pass[Int, T]).flatMap(_ => worker(name, n - 1)).map(_ + 1)
  test("import okay.effects.concurrent._") {
    assertEquals(run(writer[String](scheduled[Int](List(worker("a", 2), worker("b", 2))))), (List("a2", "b2", "a1", "b1"), List(Some(2), Some(2))))
  }
}
