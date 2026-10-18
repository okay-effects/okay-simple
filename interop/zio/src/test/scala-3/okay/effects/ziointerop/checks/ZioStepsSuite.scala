package okay.effects.ziointerop.checks

import okay.effects.*
import okay.effects.ziointerop.*
import zio.{Runtime, Unsafe, ZEnvironment, ZIO}

/** ZIO's own types as effects: one member per ZIO type, the row's environment and error read off the members */
class ZioStepsSuite extends munit.FunSuite:
  trait Db { def n: Int }
  trait Log { def tag: String }
  sealed trait AppErr
  final case class DbErr(m: String) extends AppErr
  final case class LogErr(m: String) extends AppErr

  def unsafeRun[E, A](z: ZIO[Any, E, A]): Either[E, A] =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z.either).getOrThrowFiberFailure())

  type Row[X] = (ZioStep[Db, DbErr] + ZioStep[Log, LogErr])[X]
  val env: ZEnvironment[Db & Log] = ZEnvironment[Db, Log](new Db { def n = 2 }, new Log { def tag = "abc" })
  val z1: ZIO[Db, DbErr, Int] = ZIO.serviceWith[Db](_.n)
  val z2: ZIO[Log, LogErr, Int] = ZIO.serviceWith[Log](_.tag.length)
  val p: Int ! Row = z1.step[Row].flatMap(a => z2.step[Row].map(_ + a))

  test("toZIOSteps: the type ZIO's own for gives") {
    val own: ZIO[Db & Log, AppErr, Int] = for a <- z1; b <- z2 yield a + b
    val ours: ZIO[Db & Log, DbErr | LogErr, Int] = p.toZIOSteps
    val widened: ZIO[Db & Log, AppErr, Int] = ours
    assertEquals(unsafeRun(widened.provideEnvironment(env)), unsafeRun(own.provideEnvironment(env)))
    assertEquals(unsafeRun(ours.provideEnvironment(env)), Right(5))
  }

  test("provideSteps: the environment gone from every step") {
    val q: Int ! ZioStep[Any, DbErr | LogErr] = p.provideSteps(env)
    assertEquals(unsafeRun(q.toZIOSteps), Right(5))
  }

  test("mapErrorSteps: the first failure stops the program, mapped") {
    var after = false
    val down: ZIO[Db, DbErr, Int] = ZIO.fail(DbErr("down"))
    val failing: Int ! Row = down.step[Row].flatMap(a => z2.step[Row].map { b => after = true; a + b })
    val q: Int ! ZioStep[Db & Log, String] = failing.mapErrorSteps(_.toString)
    assertEquals(unsafeRun(q.toZIOSteps.provideEnvironment(env)), Left("DbErr(down)"))
    assert(!after)
  }

  test("three members, the steps in their order") {
    type Three[X] = (ZioStep[Db, DbErr] + ZioStep[Log, LogErr] + ZioStep[Any, Nothing])[X]
    var seen = List.empty[String]
    def note(s: String): ZIO[Any, Nothing, Unit] = ZIO.succeed { seen = s :: seen }
    val q: Int ! Three = for
      _ <- note("a").step[Three]
      a <- z1.step[Three]
      _ <- note("b").step[Three]
      b <- z2.step[Three]
      _ <- note("c").step[Three]
    yield a + b
    assertEquals(unsafeRun(q.toZIOSteps.provideEnvironment(env)), Right(5))
    assertEquals(seen.reverse, List("a", "b", "c"))
  }

  test("a thousand steps") {
    type One[X] = ZioStep[Any, Nothing][X]
    val q: Int ! One = (1 to 1000).foldLeft(ZIO.succeed(0).step[One])((m, _) => m.flatMap(x => ZIO.succeed(x + 1).step[One]))
    assertEquals(unsafeRun(q.toZIOSteps), Right(1000))
  }

  test("the program run twice, each its own ZIO") {
    type One[X] = ZioStep[Any, Nothing][X]
    val q: Int ! One = ZIO.succeed(20).step[One].flatMap(x => ZIO.succeed(x + 1).step[One])
    assertEquals(unsafeRun(q.toZIOSteps.zipWith(q.toZIOSteps)(_ + _)), Right(42))
  }
