package defaults

import scala.concurrent.Future
import okay.effects._
import okay.effects.async._
import okay.effects.stream._

class AsyncDefaults extends munit.FunSuite {
  private implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
  test("import okay.effects.async._: a program waiting, driven to a future") {
    toFuture(sleep[Async](5).flatMap(_ => fromFuture[Int, Async](Future.successful(41))).map(_ + 1))
      .map(v => assertEquals(v, 42))
  }
  test("streams of the default stream module merged as they come") {
    def ticks(name: String, ms: Long, n: Int): Stream[String, Async] = range[Async](1, n + 1).evalMap(i => sleep[Async](ms).map(_ => s"$name$i"))
    toFuture(mergeAsync(List(ticks("a", 150, 2), ticks("b", 20, 2))).toList).map(xs => assertEquals(xs, List("b1", "b2", "a1", "a2")))
  }
}
