package defaults

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import okay.effects._
import okay.effects.std._
import okay.effects.stream._
import okay.effects.fs2interop._

class Fs2Defaults extends munit.FunSuite {
  private implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
  type LIO[X] = Lifted[IO]#L[X]
  test("import okay.effects.fs2interop._: the default streams to fs2 and back") {
    val s: Stream[Int, LIO] = range[LIO](1, 4).evalMap(n => lift[IO, Int, LIO](IO(n * 10)))
    toFs2(s).compile.toList.unsafeToFuture().map(xs => assertEquals(xs, List(10, 20, 30)))
  }
}
