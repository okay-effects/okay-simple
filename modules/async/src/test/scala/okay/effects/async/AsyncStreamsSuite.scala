package okay.effects.async.checks

import okay.effects.async.{Async, AsyncStreams}

import scala.concurrent.{ExecutionContext, Future}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class AsyncStreamsChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val a = AsyncStreams()
  import a._
  import a.fx._
  import a.streams._
  import a.asynchrony.{fromFuture, sleep, toFuture}

  /** `n` elements, one each `ms` milliseconds */
  def ticks(name: String, ms: Long, n: Int): Stream[String, Async] =
    range[Async](1, n + 1).evalMap(i => sleep[Async](ms).as(s"$name$i"))

  test("merged as they come: the faster stream's elements first") {
    toFuture(mergeAsync(List(ticks("a", 200, 2), ticks("b", 20, 3))).toList).map(xs =>
      assertEquals(xs, List("b1", "b2", "b3", "a1", "a2")))
  }
  test("the streams one by one: mergeAsync(a, b, c)") {
    toFuture(mergeAsync(emits[Int, Async](List(1, 2)), emits[Int, Async](List(10)), empty[Int, Async]).toList).map(xs =>
      assertEquals(xs.sorted, List(1, 2, 10)))
  }
  test("streams with no wait: every element, each stream's in its order") {
    toFuture(mergeAsync(List(emits[Int, Async](List(1, 2, 3)), emits[Int, Async](List(10, 20)), empty[Int, Async])).toList).map { xs =>
      assertEquals(xs.sorted, List(1, 2, 3, 10, 20))
      assertEquals(xs.filter(_ < 10), List(1, 2, 3))
      assertEquals(xs.filter(_ >= 10), List(10, 20))
    }
  }
  test("an endless stream merged, taken as far as asked") {
    val endless = repeatEval[String, Async](fromFuture[String, Async](Future.successful("x")))
    toFuture(mergeAsync(List(endless, ticks("b", 5, 2))).take(5).toList).map(xs => assertEquals(xs.length, 5))
  }
}

class AsyncStreamsOnMachine extends AsyncStreamsChecks(Machine)
class AsyncStreamsOnFree extends AsyncStreamsChecks(Free)
class AsyncStreamsOnTagless extends AsyncStreamsChecks(okay.effects.tagless.Tagless.on(Machine))
