package okay.effects.zio

import okay.effects.Effects
import okay.effects.machine.Machine
import okay.effects.free.Free
import zio.{Runtime, Unsafe, ZIO}

/** ZIO over the effects, on an implementation */
abstract class ZioChecks(val fx0: Effects) extends munit.FunSuite {
  val z = ZioEffects(fx0)
  import z._
  import z.fx._

  private def unsafeRun[A](task: zio.Task[A]): A = Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(task).getOrThrow() }

  test("a program of ZIO tasks, lowered to a task, run by ZIO's runtime") {
    val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIO[Int, Tasks](ZIO.attempt(a + 22)))
    assertEquals(unsafeRun(toZIO(p)), 42)
  }
  test("a task failed: the program's failure ZIO's") {
    val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.fail(new RuntimeException("boom"))).map(_ + 1)
    assertEquals(unsafeRun(toZIO(p).either).left.map(_.getMessage), Left("boom"))
  }
}

class ZioOnMachine extends ZioChecks(Machine)
class ZioOnFree extends ZioChecks(Free)
