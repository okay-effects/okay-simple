package okay.effects

import okay.effects.Places.{+, Place}
import okay.effects.free.monad.{At, Free}
import okay.effects.free.monad.Free.!

/** the `Free` monad's own handler, on its own: an operation of the last effect to its clause, any other passed on */
class FreeHandleSuite extends munit.FunSuite:
  enum Ask[X]:
    case Number extends Ask[Int]
  enum Tell[X]:
    case Say(s: String) extends Tell[Unit]

  type R[X] = (Void + Tell + Ask)[X]

  val program: Int ! R =
    Free.Bind(Free.Perform(At.Placed(Place.Init[Tell, Void + Tell, Ask, Void](Place.Last()), Tell.Say("hi"))), (_: Unit) =>
      Free.Bind(Free.Perform(At.Placed(Place.Last[Void + Tell, Ask](), Ask.Number)), (n: Int) => Free.Return(n + 1)))

  def asking[A](p: A ! R): A ! (Void + Tell) = Free.handle(p)(a => Free.Return(a),
    [X] => (op: Ask[X], k: X => A ! (Void + Tell)) => op match
      case Ask.Number => k(41))

  def telling[A](p: A ! (Void + Tell)): (List[String], A) ! Void = Free.handle(p)(a => Free.Return((List.empty[String], a)),
    [X] => (op: Tell[X], k: X => (List[String], A) ! Void) => op match
      case Tell.Say(s) => k(()).map { case (said, a) => (s :: said, a) })

  /** a state of its own: each number one more than the last */
  def counting[A](p: A ! (Void + Ask)): A ! Void = Free.handle(p, 0)((_, a) => Free.Return(a),
    [X] => (n: Int, op: Ask[X], k: (X, Int) => A ! Void) => op match
      case Ask.Number => k(n, n + 1))

  val number: Int ! (Void + Ask) = Free.Perform(At.Placed(Place.Last[Void, Ask](), Ask.Number))
  def numbers(n: Int): List[Int] ! (Void + Ask) =
    if n == 0 then Free.Return(Nil) else number.flatMap(x => numbers(n - 1).map(x :: _))

  test("the last effect handled, the other passed on, then handled outside"):
    assertEquals(Free.run(telling(asking(program))), (List("hi"), 42))

  test("a state of its own, k resumed with the next"):
    assertEquals(Free.run(counting(numbers(3))), List(0, 1, 2))

  test("constant stack: 100 000 operations resumed at once"):
    assertEquals(Free.run(counting(numbers(100000).map(_.length))), 100000)
