package okay.effects.logic.checks

import okay.effects.logic.{Logic, Term}

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free
import Term._

abstract class LogicChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val lg = Logic()
  import lg._
  import lg.fx._

  type L[X] = Sum[({ type A[x] = Sum[({ type B[y] = Sum[Void, Std.Choose, y] })#B, Std.Fresh, x] })#A, Bindings, X]

  /** `out` is `l` and then `s`: the relation, run either way */
  def appendo(l: Term, s: Term, out: Term): Unit ! L =
    either[Unit, L](
      unify[L](l, Term.empty).flatMap(_ => unify[L](s, out)),
      variable[L].flatMap(h =>
        variable[L].flatMap(t =>
          variable[L].flatMap(res =>
            unify[L](l, Pair(h, t)).flatMap(_ => unify[L](out, Pair(h, res))).flatMap(_ => appendo(t, s, res))))))

  val one = Atom("1"); val two = Atom("2"); val three = Atom("3")

  test("forwards: one list and another") {
    val q = variable[L].flatMap(out => appendo(list(one), list(two, three), out).flatMap(_ => resolved[L](out)))
    assertEquals(lg.fx.run(solutions(q)), List(list(one, two, three)))
  }
  test("backwards: every way to split a list in two") {
    val q = variable[L].flatMap(l =>
      variable[L].flatMap(s =>
        appendo(l, s, list(one, two, three)).flatMap(_ => resolved[L](Pair(l, s)))))
    assertEquals(lg.fx.run(solutions(q)),
      List(
        Pair(Term.empty, list(one, two, three)), Pair(list(one), list(two, three)),
        Pair(list(one, two), list(three)), Pair(list(one, two, three), Term.empty)))
  }
  test("no answer where the terms cannot be the same") {
    assertEquals(lg.fx.run(solutions(unify[L](one, two))), Nil)
  }

  type C[X] = Sum[Void, Std.Choose, X]
  /** a search whose first way never ends: 0, 1, 2, ... forever */
  def nats(n: Int): Int ! C = either[Int, C](nats(n + 1), pure[Int, C](n))
  test("a fair search finds what a depth-first one never would") {
    assertEquals(lg.fx.run(fair(3)(either[Int, C](nats(100), pure[Int, C](-1)))), List(-1, 100, 101))
  }
}

class LogicOnMachine extends LogicChecks(Machine)
class LogicOnFree extends LogicChecks(Free)
class LogicOnTagless extends LogicChecks(okay.effects.tagless.Tagless.on(Machine))
