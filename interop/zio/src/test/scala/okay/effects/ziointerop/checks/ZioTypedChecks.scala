package okay.effects.ziointerop.checks

import scala.concurrent.{ExecutionContext, Future}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free
import okay.effects.ziointerop.ZioEffects
import zio.{Runtime, Unsafe, ZEnvironment, ZIO, ZLayer}

trait Greeting { def greet(name: String): String }

/** ZIO typed: its environment our Reader, its error our Error, its run our Async */
abstract class ZioTypedChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val z = ZioEffects()
  import z._
  import z.fx._
  import z.fx.std._
  import z.asynchrony.toFuture

  type Row[X] = ZioRow[Greeting, String]#L[X]
  val hello: Greeting = new Greeting { def greet(name: String) = s"hello, $name" }
  def unsafeRun[A](task: zio.Task[A]): Future[A] = Unsafe.unsafe { implicit u => Runtime.default.unsafe.runToFuture(task) }

  /** run here: the reader given, the error attempted, the async driven to a future */
  def here[A](p: A ! Row): Future[Either[String, A]] = toFuture(attempt[String](reader[ZEnvironment[Greeting]](ZEnvironment(hello))(p)))

  test("the environment our Reader") {
    val p = fromZIOTyped[Greeting, String, String, Row](ZIO.serviceWith[Greeting](_.greet("Ann")))
    here(p).map(v => assertEquals(v, Right("hello, Ann")))
  }
  test("a typed failure our Error; a defect the run's failure") {
    here(fromZIOTyped[Greeting, String, Int, Row](ZIO.fail("bad"))).map(v => assertEquals(v, Left("bad")))
      .flatMap(_ => here(fromZIOTyped[Greeting, String, Int, Row](ZIO.die(new RuntimeException("defect")))).failed)
      .map(e => assertEquals(e.getMessage, "defect"))
  }
  test("toZIOTyped: the Reader ZIO's environment, a raise ZIO's failure") {
    val p: String ! Row = ask[ZEnvironment[Greeting], Row].map(_.get[Greeting].greet("Bo"))
    val q: Int ! Row = raise[String, Row, Int]("no")
    unsafeRun(toZIOTyped(p).provideEnvironment(ZEnvironment(hello)).either).map(v => assertEquals(v, Right("hello, Bo")))
      .flatMap(_ => unsafeRun(toZIOTyped(q).provideEnvironment(ZEnvironment(hello)).either))
      .map(v => assertEquals(v, Left("no")))
  }
  test("cancelled here: the ZIO's fiber interrupted") {
    val interrupted = scala.concurrent.Promise[Unit]()
    val never: ZIO[Greeting, String, Int] = ZIO.never.onInterrupt(ZIO.succeed { interrupted.trySuccess(()); () })
    val p: Option[Int] ! okay.effects.async.Async = z.asynchrony.timeout(30)(attempt[String](reader[ZEnvironment[Greeting]](ZEnvironment(hello))(
      fromZIOTyped[Greeting, String, Int, Row](never)))).map(_.map(_.getOrElse(-1)))
    for { t <- toFuture(p); _ <- interrupted.future } yield assertEquals(t, None)
  }
  test("interrupted in ZIO: the program cancelled, the wait it was in cancelled") {
    val stopped = scala.concurrent.Promise[Unit]()
    val waiting: Int ! Row = z.asynchrony.cancellable[Int, Row](_ => () => { stopped.trySuccess(()); () })
    for {
      r <- unsafeRun(toZIOTyped(waiting).provideEnvironment(ZEnvironment(hello)).timeout(zio.Duration.fromMillis(30)).either)
      _ <- stopped.future
    } yield assertEquals(r, Right(None))
  }
  test("layers: a program's environment built by a layer; a program making a service a layer") {
    type NoEnv[X] = ZioRow[Any, String]#L[X]
    val greeted: String ! Row = ask[ZEnvironment[Greeting], Row].map(_.get[Greeting].greet("Cy"))
    val provided: String ! NoEnv = provideLayer[Any, String, Greeting, String, NoEnv](ZLayer.succeed(hello))(greeted)
    val made: ZLayer[Any, String, Greeting] = layer[Any, String, Greeting](pure[Greeting, NoEnv](hello))
    toFuture(attempt[String](reader[ZEnvironment[Any]](ZEnvironment.empty)(provided))).map(v => assertEquals(v, Right("hello, Cy")))
      .flatMap(_ => unsafeRun(ZIO.serviceWith[Greeting](_.greet("Di")).provideLayer(made).either))
      .map(v => assertEquals(v, Right("hello, Di")))
  }
  test("there and back: a ZIO, a program, a ZIO") {
    val zio0: ZIO[Greeting, String, Int] = ZIO.serviceWith[Greeting](_.greet("x").length)
    unsafeRun(toZIOTyped(fromZIOTyped[Greeting, String, Int, Row](zio0)).provideEnvironment(ZEnvironment(hello)).either)
      .map(v => assertEquals(v, Right(8)))
  }
}

class ZioTypedOnMachine extends ZioTypedChecks(Machine)
class ZioTypedOnFree extends ZioTypedChecks(Free)
