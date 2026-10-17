package okay.effects.concurrent

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ConcurrentChecks(val fx0: Runner) extends munit.FunSuite {
  val c = Concurrent(fx0)
  import c._
  import c.fx._
  import c.fx.std._

  type Ch[X] = Coop[Int, X]
  type Wr[X] = Writer[String, X]
  type T[X] = Sum[({ type L[x] = Sum[Void, Wr, x] })#L, Ch, X]

  def worker(name: String, n: Int): Int ! T =
    if (n == 0) pure[Int, T](0)
    else tell[String, T](s"$name$n").flatMap(_ => pass[Int, T]).flatMap(_ => worker(name, n - 1)).map(_ + 1)

  test("threads in turns: each passes, the next runs") {
    val (log, results) = fx0.run(writer[String](scheduled[Int](List(worker("a", 2), worker("b", 3)))))
    assertEquals(log, List("a2", "b3", "a1", "b2", "b1"))
    assertEquals(results, List(Some(2), Some(3)))
  }

  def producer(xs: List[Int]): Int ! T = xs.foldLeft(pure[Int, T](0))((p, x) => p.flatMap(n => send[Int, T](x).as(n + 1)))
  def consumer(n: Int, sum: Int): Int ! T = if (n == 0) pure[Int, T](sum) else recv[Int, T].flatMap(x => consumer(n - 1, sum + x))

  test("a channel: a producer and a consumer meet at each value") {
    assertEquals(fx0.run(writer[String](scheduled[Int](List(producer(List(1, 2, 3)), consumer(3, 0)))))._2, List(Some(3), Some(6)))
  }
  test("a receiver no one sends to waits forever: None") {
    assertEquals(fx0.run(writer[String](scheduled[Int](List(consumer(1, 0), pure[Int, T](5)))))._2, List(None, Some(5)))
  }
  test("many threads, many turns, in constant stack") {
    val r = fx0.run(writer[String](scheduled[Int]((1 to 50).toList.map(_ => worker("x", 200)))))
    assertEquals(r._2.flatten.sum, 50 * 200)
  }
}

class ConcurrentOnMachine extends ConcurrentChecks(Machine)
class ConcurrentOnFree extends ConcurrentChecks(Free)
class ConcurrentOnTagless extends ConcurrentChecks(okay.effects.tagless.Tagless.on(Machine))
