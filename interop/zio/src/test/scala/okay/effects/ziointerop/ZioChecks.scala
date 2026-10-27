package okay.effects.ziointerop.checks

import okay.effects.ziointerop.{ZioEffects}

import okay.effects.{Runner, Std, Sum}
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
    val p: Int ! Tasks =
      fromZIOTask[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIOTask[Int, Tasks](ZIO.attempt(a + 22)))
    unsafeRun(toZIOTask(p)).map(v => assertEquals(v, 42))
  }
  test("a task failed: the program's failure ZIO's") {
    val p: Int ! Tasks = fromZIOTask[Int, Tasks](ZIO.fail(new RuntimeException("boom"))).map(_ + 1)
    unsafeRun(toZIOTask(p).either).map(v => assertEquals(v.left.map(_.getMessage), Left("boom")))
  }
  test("liftVia: ZIO tasks as Futures, one control monad lowered") {
    type LF[X] = Std.Lift[scala.concurrent.Future, X]
    type R[X] = Sum[Tasks, LF, X]
    implicit val futureLowering: Std.Lowering[scala.concurrent.Future] = new Std.Lowering[scala.concurrent.Future] {
      def pure[A](a: A): scala.concurrent.Future[A] = scala.concurrent.Future.successful(a)
      def flatMap[A, B](ga: scala.concurrent.Future[A])(f: A => scala.concurrent.Future[B]) = ga.flatMap(f)
    }
    val p: Int ! R = std.lift[scala.concurrent.Future, Int, R](scala.concurrent.Future.successful(20))
      .flatMap(a => fromZIOTask[Int, R](ZIO.attempt(a + 22)))
    val f = lowerLast[scala.concurrent.Future, ({ type L[x] = Sum[Pure, LF, x] })#L, Int](
      std.liftVia[zio.Task, scala.concurrent.Future](taskToFuture(Runtime.default))(p))
    f.map(v => assertEquals(v, 42))
  }
}
