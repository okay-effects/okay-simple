package okay.effects.stream.checks

import okay.effects.stream.{Streams}

import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class StreamsChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val st = Streams()
  import st._
  import st.fx._
  import st.fx.std._

  type P[X] = Pure[X]
  def list[O](s: Stream[O, P]): List[O] = st.fx.run(s.toList)

  test("map, filter, take, drop") {
    assertEquals(list(range[P](0, 10).map(_ * 3).filter(_ % 2 == 0).drop(1).take(3)), List(6, 12, 18))
  }
  test("an endless stream, taken as far as asked") {
    assertEquals(list(iterate[Int, P](1)(_ * 2).take(5)), List(1, 2, 4, 8, 16))
    assertEquals(list(iterate[Int, P](0)(_ + 1).takeWhile(_ < 4)), List(0, 1, 2, 3))
  }
  test("zip, interleave, ++, flatMap, scan, zipWithIndex, unfold") {
    assertEquals(list(emits[String, P](List("a", "b", "c")).zip(iterate[Int, P](1)(_ + 1))), List(("a", 1), ("b", 2), ("c", 3)))
    assertEquals(list(emits[Int, P](List(1, 3, 5)).interleave(emits[Int, P](List(2, 4, 6, 8, 10)))), List(1, 2, 3, 4, 5, 6, 8, 10))
    assertEquals(list(range[P](0, 2) ++ range[P](5, 7)), List(0, 1, 5, 6))
    assertEquals(list(range[P](1, 4).flatMap(n => emits[Int, P](List.fill(n)(n)))), List(1, 2, 2, 3, 3, 3))
    assertEquals(list(range[P](1, 5).scan(0)(_ + _)), List(0, 1, 3, 6, 10))
    assertEquals(list(emits[String, P](List("x", "y")).zipWithIndex), List(("x", 0L), ("y", 1L)))
    assertEquals(list(unfold[Int, Int, P](10)(n => if (n == 0) None else Some((n, n / 2)))), List(10, 5, 2, 1))
  }

  test("merge: one of each in turn, an ended one left out") {
    assertEquals(list(merge[String, P](List(emits[String, P](List("a1", "a2", "a3")), emits[String, P](List("b1")),
      emits[String, P](List("c1", "c2"))))), List("a1", "b1", "c1", "a2", "c2", "a3"))
    assertEquals(list(merge[Int, P](Nil)), Nil)
    assertEquals(list(merge[Int, P](List(iterate[Int, P](0)(_ + 2), iterate[Int, P](1)(_ + 2))).take(6)), List(0, 1, 2, 3, 4, 5))
  }
  type St[X] = State[Int, X]
  type Wr[X] = Writer[String, X]
  type SW[X] = Sum[({ type L[x] = Sum[Void, Wr, x] })#L, St, X]
  test("merge pulls each stream only as far as asked, its effects in turn") {
    def counted(name: String): Stream[String, St] = repeatEval[String, St](modify[Int, St](_ + 1).flatMap(_ => get[Int, St]).map(n => s"$name$n"))
    assertEquals(st.fx.run(state(0)(merge[String, St](List(counted("a"), counted("b"))).take(4).toList)), (4, List("a1", "b2", "a3", "b4")))
  }
  test("effects in a stream: each element made by one, each pull only as far as asked") {
    val counted = repeatEval[Int, St](modify[Int, St](_ + 1).flatMap(_ => get[Int, St])).take(3).toList
    assertEquals(st.fx.run(state(0)(counted)), (3, List(1, 2, 3)))
  }
  test("evalMap and foreach: effects of the row, in order") {
    val p = range[SW](1, 4).evalMap(n => modify[Int, SW](_ + n).flatMap(_ => get[Int, SW])).foreach(t => tell[String, SW](s"total $t"))
    assertEquals(st.fx.run(writer[String](state(0)(p))), (List("total 1", "total 3", "total 6"), (6, ())))
  }

  type Y[X] = Yield[Int, X]
  def nums(n: Int): Unit ! Y = if (n > 3) pure[Unit, Y](()) else emit[Int, Y](n).flatMap(_ => nums(n + 1))
  def naturals(n: Int): Unit ! Y = emit[Int, Y](n).flatMap(_ => naturals(n + 1))
  test("a generator as a stream, and a stream as a generator") {
    assertEquals(list(fromGenerator[Int](nums(1)).map(_ * 10)), List(10, 20, 30))
    assertEquals(list(fromGenerator[Int](naturals(0)).filter(_ % 3 == 0).take(4)), List(0, 3, 6, 9))
    assertEquals(st.fx.run(collect[Int](range[P](0, 3).map(_ + 100).emitAll[Y])), (List(100, 101, 102), ()))
  }
  test("in constant stack: 100 000 elements") {
    assertEquals(st.fx.run(range[P](0, 100000).map(_ + 1).filter(_ % 2 == 0).fold(0L)(_ + _)), (1 to 100000).filter(_ % 2 == 0).map(_.toLong).sum)
  }
}

class StreamsOnMachine extends StreamsChecks(Machine)
class StreamsOnFree extends StreamsChecks(Free)
class StreamsOnTagless extends StreamsChecks(okay.effects.tagless.Tagless.on(Machine))
