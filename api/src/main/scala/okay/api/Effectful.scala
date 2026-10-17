package okay.api

/**
 * A HANDLER'S CLAUSES, the same for both compilers: of the operations `O`, the body's value `A`, the programs `P` it
 * makes — `B ! R`, of the row outside. Each compiler's `Effects` names them by its own effects and rows:
 * `Handler[E, R, A, B]` is `Clauses[E's operations, A, B ! R]`.
 */
trait Clauses[O[_], A, P] {
  def ret(a: A): P
  /** an operation, and `k`: the rest, under this handler, resumable any number of times */
  def op[X](op: O[X], k: X => P): P
}
/** clauses each resuming `k` once, at once, with an answer: an operation ANSWERED IN PLACE (`Reader`) */
trait TailClauses[O[_], A, P] {
  def ret(a: A): P
  def answer[X](op: O[X]): X
}
/** the same with a state of its own, `cell` (`State`, `Writer`) */
trait StatefulClauses[O[_], S, A, P] {
  def ret(s: S, a: A): P
  def answer[X](cell: Cell[S], op: O[X]): X
}

/**
 * WHAT BOTH INTERFACES SHARE, in one source: everything that does not name a row. A row is each compiler's own — a
 * proper type in Scala 2, a type constructor in Scala 3 — and so is every type over it (`!`, `Split`, `In`, `Sub`)
 * and every primitive taking one (`pure`, `perform`, `handle`, `run`): those are in each `Effects`. Here: a
 * handler's three shapes (`Clauses`, `TailClauses`, `StatefulClauses`), a stateful handler's `Cell`, and how a
 * handler answering in place is a general one — the interface's defaults, for an implementation with no such forms.
 */
trait Effectful {
  /** a stateful handler's state where an operation is (`okay.api.Cell`) */
  type Cell[S] = okay.api.Cell[S]

  /** a handler answering in place as a general one: its answer given to `k` when the run gets there (`delay`) */
  protected final def tailAsGeneral[O[_], A, P](h: TailClauses[O, A, P])(delay: (=> P) => P): Clauses[O, A, P] =
    new Clauses[O, A, P] {
      def ret(a: A): P = h.ret(a)
      def op[X](op: O[X], k: X => P): P = delay(k(h.answer(op)))
    }

  /** a stateful handler as a general one of functions of the state: each clause a program `Q` of a function from the
   * state of its time, its cell that state, `k`'s function given the state the clause left. `pure` makes a `Q` of a
   * function; `bind` runs a `Q` and gives its function to the rest */
  protected final def statefulAsGeneral[O[_], S, A, P, Q](h: StatefulClauses[O, S, A, P])(pure: (S => P) => Q,
      bind: (Q, (S => P) => P) => P): Clauses[O, A, Q] =
    new Clauses[O, A, Q] {
      def ret(a: A): Q = pure((s: S) => h.ret(s, a))
      def op[X](op: O[X], k: X => Q): Q = pure { (s: S) =>
        val cell = new Effectful.VarCell[S](s)
        val x = h.answer(cell, op)
        bind(k(x), (f: S => P) => f(cell.get))
      }
    }
}

object Effectful {
  private final class VarCell[S](private var s: S) extends Cell[S] {
    def get: S = s
    def set(t: S): Unit = s = t
  }
}
