package defaults

import zio.{Runtime, Unsafe, ZIO}
import zio.stream.ZStream
import okay.effects._
import okay.effects.stream._
import okay.effects.ziointerop.{Tasks, fromZIOTask, toZIOTask}
import okay.effects.zstream._

class ZioStreamDefaults extends munit.FunSuite {
  private implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
  private def unsafe[A](task: zio.Task[A]) = Unsafe.unsafe { implicit u => Runtime.default.unsafe.runToFuture(task) }
  test("import okay.effects.ziointerop._ and okay.effects.zstream._: one stream module, one ZIO") {
    val s: Stream[Int, Tasks] = range[Tasks](1, 4).evalMap(n => fromZIOTask[Int, Tasks](ZIO.succeed(n)))
    unsafe(toZStream(s).runCollect).map(c => assertEquals(c.toList, List(1, 2, 3)))
      .flatMap(_ => unsafe(toZIOTask(fromZStream(ZStream(5, 6)).toList)))
      .map(xs => assertEquals(xs, List(5, 6)))
  }
}
