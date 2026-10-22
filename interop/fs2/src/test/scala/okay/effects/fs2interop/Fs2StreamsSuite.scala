package okay.effects.fs2interop.checks

import okay.effects.fs2interop.{Fs2Streams}

import scala.concurrent.ExecutionContext
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class Fs2StreamsChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val f: Fs2Streams[runner.type, okay.effects.stream.Streams[runner.type]] = Fs2Streams()
  import f._
  import f.fx._
  import f.fx.std._
  import f.streams._

  type LIO[X] = Lifted[IO]#L[X]
  private implicit val ioLowering: okay.effects.Std.Lowering[IO] = f.catsInterop.lowering[IO]
  def lowered[A](p: A ! LIO): IO[A] = f.fx.lower[IO, A](p)

  test("a stream of IO's as an fs2 stream: each element an IO's") {
    val s = range[LIO](1, 5).evalMap(n => lift[IO, Int, LIO](IO(n * 10)))
    toFs2(s).compile.toList.unsafeToFuture().map(xs => assertEquals(xs, List(10, 20, 30, 40)))
  }
  test("a stream of no effect as an fs2 stream, an endless one taken on fs2's side") {
    assertEquals(toFs2Pure(iterate[Int, Pure](1)(_ * 2)).take(5).toList, List(1, 2, 4, 8, 16))
  }
  test("an fs2 stream as a stream: a chunk at a time, transformed here, as far as asked") {
    val s =
      fromFs2(_root_.fs2.Stream.range(0, 1000).covary[IO].chunkN(7).unchunks.map(_ * 2)).filter(_ % 3 == 0).take(5)
    lowered(s.toList).unsafeToFuture().map(xs => assertEquals(xs, List(0, 6, 12, 18, 24)))
  }
  test("an fs2 stream's effects and its failure") {
    val effects = _root_.fs2.Stream.eval(IO(1)) ++ _root_.fs2.Stream.eval(IO(2))
    val failing = _root_.fs2.Stream.emit(1).covary[IO] ++ _root_.fs2.Stream.raiseError[IO](new RuntimeException("boom"))
    lowered(fromFs2(effects).fold(0)(_ + _)).unsafeToFuture().map(n => assertEquals(n, 3))
      .flatMap(_ => lowered(fromFs2(failing).toList).attempt.unsafeToFuture())
      .map(e => assertEquals(e.left.map(_.getMessage), Left("boom")))
  }
}

class Fs2StreamsOnMachine extends Fs2StreamsChecks(Machine)
class Fs2StreamsOnFree extends Fs2StreamsChecks(Free)
class Fs2StreamsOnTagless extends Fs2StreamsChecks(okay.effects.tagless.Tagless.on(Machine))
