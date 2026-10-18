package okay.effects.catsinterop.checks

import okay.effects.catsinterop.{Cats}

import cats.Traverse
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.instances.list._
import cats.instances.option._
import okay.effects.{Runner, Std}
import okay.effects.machine.Machine
import okay.effects.free.Free

/** cats over the effects, on an implementation */
abstract class CatsChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext
  implicit val runner: Runner = fx0
  val c: Cats[runner.type] = Cats()
  import c._
  import c.fx._
  import c.fx.std._

  type St[X] = Std.State[Int, X]
  /** cats' monad of the programs over `St`, found where cats asks for it */
  implicit val stMonad: cats.Monad[Program[St]#L] = monad[St]
  test("cats' monad over the programs: a traverse, a state summed") {
    val p = Traverse[List].traverse[Program[St]#L, Int, Int](List(1, 2, 3))(x => modify[Int, St](_ + x).as(x * 10))
    assertEquals(run(state(0)(p)), (6, List(10, 20, 30)))
  }
  test("cats' tailRecM in constant stack: 100 000 steps") {
    val p = stMonad.tailRecM(0)(i => if (i == 100000) pure[Either[Int, Int], St](Right(i)) else modify[Int, St](_ + 1).as(Left(i + 1)))
    assertEquals(run(state(0)(p)), (100000, 100000))
  }

  type LO[X] = Std.Lift[Option, X]
  def sum(b: Option[Int]): Int ! LO = lift[Option, Int, LO](Some(1)).flatMap(a => lift[Option, Int, LO](b).map(_ + a))
  test("a program of Option's lifted, lowered to an Option by cats' monad") {
    assertEquals(lower[Option, Int](sum(Some(2))), Some(3))
    assertEquals(lower[Option, Int](sum(None)), None)
  }

  type LIO[X] = Std.Lift[IO, X]
  test("a program of cats-effect 3's IO lifted, lowered to an IO, run") {
    val p: Int ! LIO = lift[IO, Int, LIO](IO(20)).flatMap(a => lift[IO, Int, LIO](IO(a + 22)))
    lower[IO, Int](p).unsafeToFuture().map(v => assertEquals(v, 42))
  }
}

class CatsOnMachine extends CatsChecks(Machine)
class CatsOnFree extends CatsChecks(Free)
