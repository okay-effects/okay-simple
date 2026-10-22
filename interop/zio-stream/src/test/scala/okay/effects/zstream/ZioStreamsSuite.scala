package okay.effects.zstream.checks

import okay.effects.zstream.{ZioStreams}

import scala.concurrent.{ExecutionContext, Future}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free
import zio.{Runtime, Unsafe, ZIO}
import zio.stream.ZStream

abstract class ZioStreamsChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val z = ZioStreams()
  import z._
  import z.fx._
  import z.zioEffects.{Tasks, fromZIOTask, toZIOTask}
  import z.streams._

  private def unsafeRun[A](task: zio.Task[A]): Future[A] = Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.runToFuture(task)
  }

  test("a stream of tasks as a ZStream: each element a task's") {
    val s = range[Tasks](1, 5).evalMap(n => fromZIOTask[Int, Tasks](ZIO.succeed(n * 10)))
    unsafeRun(toZStream(s).runCollect).map(c => assertEquals(c.toList, List(10, 20, 30, 40)))
  }
  test("a stream of no effect as a ZStream, an endless one taken on ZIO's side") {
    unsafeRun(toZStreamPure(iterate[Int, Pure](1)(_ * 3)).take(4).runCollect).map(c =>
      assertEquals(c.toList, List(1, 3, 9, 27)))
  }
  test("a ZStream as a stream: transformed here, as far as asked") {
    val s = fromZStream(ZStream.range(0, 100).rechunk(7).map(_ * 2)).filter(_ % 3 == 0).take(5)
    unsafeRun(toZIOTask(s.toList)).map(xs => assertEquals(xs, List(0, 6, 12, 18, 24)))
  }
  test("a ZStream read to its end; its failure the task's") {
    unsafeRun(toZIOTask(fromZStream(ZStream(1, 2, 3)).fold(0)(_ + _))).map(n => assertEquals(n, 6))
      .flatMap(_ =>
        unsafeRun(toZIOTask(fromZStream(ZStream(1) ++ ZStream.fail(new RuntimeException("boom"))).toList).either))
      .map(e => assertEquals(e.left.map(_.getMessage), Left("boom")))
  }
}

class ZioStreamsOnMachine extends ZioStreamsChecks(Machine)
class ZioStreamsOnFree extends ZioStreamsChecks(Free)
class ZioStreamsOnTagless extends ZioStreamsChecks(okay.effects.tagless.Tagless.on(Machine))
