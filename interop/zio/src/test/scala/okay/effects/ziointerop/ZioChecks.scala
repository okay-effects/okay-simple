package okay.effects.ziointerop.checks

import okay.effects.ziointerop.{ZioEffects}

import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free
import zio.{Runtime, Unsafe, ZIO}

/** ZIO over the effects, on an implementation */
abstract class ZioChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val z = ZioEffects()
  import z._
  import z.fx._

  /** the task run by ZIO's runtime, its value a `Future`: munit waits for it, on the JVM and on Scala.js */
  private def unsafeRun[A](task: zio.Task[A]): scala.concurrent.Future[A] =
    Unsafe.unsafe { implicit u => Runtime.default.unsafe.runToFuture(task) }

  test("a program of ZIO tasks, lowered to a task, run by ZIO's runtime") {
    val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIO[Int, Tasks](ZIO.attempt(a + 22)))
    unsafeRun(toZIO(p)).map(v => assertEquals(v, 42))
  }
  test("a task failed: the program's failure ZIO's") {
    val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.fail(new RuntimeException("boom"))).map(_ + 1)
    unsafeRun(toZIO(p).either).map(v => assertEquals(v.left.map(_.getMessage), Left("boom")))
  }
}

class ZioOnMachine extends ZioChecks(Machine)
class ZioOnFree extends ZioChecks(Free)
