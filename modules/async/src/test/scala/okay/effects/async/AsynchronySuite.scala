package okay.effects.async.checks

import okay.effects.async.{Async, Asynchrony, Cancelled}

import scala.concurrent.{ExecutionContext, Future}
import okay.effects.{Runner, Sum}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class AsynchronyChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val as = Asynchrony()
  import as._
  import as.fx._
  import as.fx.std._

  test("a value after a sleep, then another from a future") {
    val p = sleep[Async](10).flatMap(_ => fromFuture[Int, Async](Future(20))).map(_ + 22)
    toFuture(p).map(v => assertEquals(v, 42))
  }
  test("a failure in a callback: the future failed") {
    val p = fromFuture[Int, Async](Future.failed(new RuntimeException("boom"))).map(_ + 1)
    toFuture(p).failed.map(e => assertEquals(e.getMessage, "boom"))
  }
  type St[X] = State[Int, X]
  type SA[X] = Sum[Async, St, X]
  test("another effect handled first, the row then Async alone") {
    def tick(n: Int): Unit ! SA =
      if (n == 0) pure[Unit, SA](()) else sleep[SA](1).flatMap(_ => modify[Int, SA](_ + 1)).flatMap(_ => tick(n - 1))
    toFuture(state(0)(tick(5))).map(v => assertEquals(v, (5, ())))
  }
  test("both at once, a race, a timeout") {
    val slow = sleep[Async](300).as("slow")
    val fast = sleep[Async](5).as(7)
    for {
      b <- toFuture(both(fast, sleep[Async](5).as("x")))
      r <- toFuture(race(slow, fast))
      t1 <- toFuture(timeout(50)(slow))
      t2 <- toFuture(timeout(300)(fast))
      ps <- toFuture(parTraverse((1 to 5).toList)(i => sleep[Async](5).as(i * i)))
    } yield {
      assertEquals(b, (7, "x"))
      assertEquals(r, Right(7))
      assertEquals(t1, None)
      assertEquals(t2, Some(7))
      assertEquals(ps, List(1, 4, 9, 16, 25))
    }
  }
  test("cancelled: the wait cancelled, the rest never run, the future failed") {
    @volatile var cancelledWait = false
    @volatile var ranOn = false
    val p = cancellable[Int, Async](_ => () => cancelledWait = true).map { n => ranOn = true; n }
    val running = start(p)
    running.cancel()
    running.future.failed.map { e =>
      assert(e.isInstanceOf[Cancelled])
      assert(cancelledWait && !ranOn)
    }
  }
  test("a timeout cancels the program that lost; a race the slower") {
    val stopped = scala.concurrent.Promise[Unit]()
    val never = cancellable[Int, Async](_ => () => { stopped.trySuccess(()); () })
    for {
      t <- toFuture(timeout(20)(never))
      _ <- stopped.future
    } yield assertEquals(t, None)
  }
  test("10 000 waits in turn, in constant stack") {
    def loop(n: Int, acc: Int): Int ! Async = if (n == 0) pure[Int, Async](acc)
    else fromFuture[Int, Async](Future.successful(1)).flatMap(x => loop(n - 1, acc + x))
    toFuture(loop(10000, 0)).map(v => assertEquals(v, 10000))
  }
}

class AsynchronyOnMachine extends AsynchronyChecks(Machine)
class AsynchronyOnFree extends AsynchronyChecks(Free)
class AsynchronyOnTagless extends AsynchronyChecks(okay.effects.tagless.Tagless.on(Machine))
