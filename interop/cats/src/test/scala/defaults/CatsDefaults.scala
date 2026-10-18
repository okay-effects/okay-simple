package defaults

import _root_.cats.Monad
import okay.effects._
import okay.effects.std._
import okay.effects.catsinterop._

class CatsDefaults extends munit.FunSuite {
  type St[X] = State[Int, X]
  def twice[F[_]](fa: F[Int])(implicit F: Monad[F]): F[Int] = F.flatMap(fa)(a => F.map(fa)(_ + a))
  test("import okay.effects.catsinterop._: cats' monad of the programs, found") {
    assertEquals(run(state(0)(twice[Program[St]#L](modify[Int, St](_ + 1).flatMap(_ => get[Int, St])))), (2, 3))
  }
}
