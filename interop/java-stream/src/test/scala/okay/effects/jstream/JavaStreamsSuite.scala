package okay.effects.jstream.checks

import okay.effects.jstream.{JavaStreams}

import java.util.stream.{Collectors, IntStream, Stream => JStream}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class JavaStreamsChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val j = JavaStreams()
  import j._
  import j.fx._
  import j.fx.std._
  import j.streams._

  test("a Java stream as a stream: transformed, as far as asked") {
    val s = fromJava[Integer, Pure](IntStream.range(0, 1000000).boxed()).map(_.intValue * 2).filter(_ % 3 == 0).take(4)
    assertEquals(run(s.toList), List(0, 6, 12, 18))
  }
  test("used once in Java, repeatable here: pulled twice, the same elements") {
    val s = fromJava[String, Pure](JStream.of("a", "b", "c"))
    assertEquals(run(s.toList), List("a", "b", "c"))
    assertEquals(run(s.toList), List("a", "b", "c"))
    val it = fromJavaIterator[Int, Pure](java.util.Arrays.asList(1, 2, 3).iterator())
    assertEquals(run(it.toList), run(it.toList))
  }
  type St[X] = State[Int, X]
  test("a Java stream's elements in a stream of effects") {
    val p = fromJava[Int, St](JStream.of(1, 2, 3)).evalMap(n => modify[Int, St](_ + n).flatMap(_ => get[Int, St])).toList
    assertEquals(run(state(0)(p)), (6, List(1, 3, 6)))
  }
  test("a stream as a Java stream: lazy, an endless one limited on Java's side, collected") {
    val js = toJava(iterate[Int, Pure](1)(_ + 1).map(n => n * n))
    assertEquals(js.limit(5).collect(Collectors.toList[Int]()).toString, "[1, 4, 9, 16, 25]")
    val it = toJavaIterator(emits[String, Pure](List("x", "y")))
    assertEquals((it.hasNext, it.next(), it.next(), it.hasNext), (true, "x", "y", false))
  }
}

class JavaStreamsOnMachine extends JavaStreamsChecks(Machine)
class JavaStreamsOnFree extends JavaStreamsChecks(Free)
class JavaStreamsOnTagless extends JavaStreamsChecks(okay.effects.tagless.Tagless.on(Machine))
