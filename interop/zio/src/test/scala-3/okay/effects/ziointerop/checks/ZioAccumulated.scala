package okay.effects.ziointerop.checks

import scala.concurrent.Future
import okay.effects.*
import okay.effects.std.*
import okay.effects.async.toFuture
import okay.effects.ziointerop.*
import zio.{Runtime, Unsafe, ZEnvironment, ZIO}

trait Db { def load(id: Int): String }
trait Log { def info(s: String): Unit }

/** ZIOs of different environments and errors in one program: the row accumulates them, as ZIO's types do */
class ZioAccumulated extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext
  type AppErr = String | Int
  type Row[X] = ZioRow[Db & Log, AppErr]#L[X]

  val db: Db = id => s"row $id"
  var logged = List.empty[String]
  val log: Log = s => logged = s :: logged

  val load: ZIO[Db, String, String] = ZIO.serviceWith[Db](_.load(7))
  def info(s: String): ZIO[Log, Int, Unit] = ZIO.serviceWith[Log](_.info(s))

  val program: Int ! Row =
    for
      row <- fromZIOTyped[Db & Log, AppErr, String, Row](load)
      _   <- fromZIOTyped[Db & Log, AppErr, Unit, Row](info(row))
    yield row.length

  test("two ZIOs of their own environments and errors: one row, one ZIO of both") {
    val z: ZIO[Db & Log, AppErr, Int] = toZIOTyped(program)
    Unsafe.unsafe { implicit u => Runtime.default.unsafe.runToFuture(z.provideEnvironment(ZEnvironment[Db, Log](db, log)).either) }
      .map(n => assertEquals((n, logged), (Right(5), List("row 7"))))
  }
