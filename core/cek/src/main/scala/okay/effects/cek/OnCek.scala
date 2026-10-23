package okay.effects.cek

import okay.effects.{AbortClauses, Clauses, Runner, Self, StatefulTailClauses, TailClauses, Void}
import okay.scala3.{At, Cek, Escape, Handler as Machines, Prog, Sub as Renaming, Taken}

/**
 * THE INTERFACE ON OKAY-SCALA'S MACHINE (github.com/okay-effects/okay-scala), a CEK machine: a `Runner` whose programs
 * are okay-scala's `Prog`, over this interface's own rows — `import okay.effects.cek.OnCek.*`, and a program written
 * against the interface runs there as it is. Neither library depends on the other's classes: okay-scala's machine
 * takes places of any representation (its `At`), and this adapter's evidence is a place of its own, `Spot`, an `At`
 * itself — typed by this interface's rows, compared by its own `take`, whose `Mine` proves by these types that an
 * operation is the handler's. Nothing wrapped, nothing converted, no cast.
 */
object OnCek extends Runner with Self:
  type ![+A, R[_]] = Prog[R, A]
  type In[E[_], R[_]] = Spot[E, R, ?]
  type Split[E[_], R[_], Rest[_]] = Spot[E, R, Rest]
  type Sub[R[_], S[_]] = Renaming[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = Spot.Only[E]()
  implicit def splitLast[R[_], E[_]]: Split[E, R + E, R] = Spot.Last[R, E]()
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Spot.Init[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Spots.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Spots.subOne(e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Spots.subMore(r, e)

  def pure[A, R[_]](a: A): Prog[R, A] = Prog.Value(a)
  def flatMap[A, B, R[_]](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Prog.FlatMap(p, f)
  override def map[A, B, R[_]](p: Prog[R, A])(f: A => B): Prog[R, B] = Prog.Map(p, f)
  def delay[A, R[_]](p: => Prog[R, A]): Prog[R, A] = Prog.Defer(() => p)
  override protected[effects] def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): Prog[R, X] =
    Prog.Perform[R, E, X](in, op)

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Rest + E, A] =
    (s: @unchecked) match
      case _: Spot.Last[r, e] => p
      case _ => Prog.Remap(p, Spots.toEnd(s))
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Prog.Remap(p, s)
  override protected[effects] def interposed[E[_], R[_], A](p: Prog[R, A])(implicit in: In[E, R]): Prog[R + E, A] =
    Prog.Remap(p, Spots.interposing(in))

  // this interface's handlers, as the machine's own of the same shape

  private def general[E[_], R[_], A, B](h: Clauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.General[E, R, A, B](a => h.ret(a), [X] => (op: E[X], k: X => Prog[R, B]) => h.op(op, k))
  private def tail[E[_], R[_], A, B](h: TailClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Tail[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.answer(op))
  private def abort[E[_], R[_], A, B](h: AbortClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Abort[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.abort(op))
  /** the machine's cell, of the adapter's kind: this interface's `Cell` too, given to the clause as it is */
  private def stateful[E[_], S, R[_], A, B](s0: S, h: StatefulTailClauses[E, S, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.stateful[E, R, A, B, S](Both(s0))((s, a) => h.ret(s, a))(
      [X] => (op: E[X], cell: okay.scala3.Cell[S]) => cell match
        case both: Both[S] @unchecked => h.answer(both, op))

  /** a cell of both: okay-scala's, which the machine keeps and copies, and this interface's, which a clause reads */
  final class Both[S](s: S) extends okay.scala3.Cell[S](s) with okay.effects.Cell[S]:
    override def fresh: okay.scala3.Cell[S] = Both(state)

  def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Spot.Last[R, E](), general(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Spot.Last[R, E](), tail(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Spot.Last[R, E](), abort(h))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B])
    : Prog[R, B] = Prog.Handle(p, Spot.Last[R, E](), stateful(s0, h))

  // a handler takes its effect from its place at once: no `split`
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: Handler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, general(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: TailHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, tail(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: AbortHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, abort(h))
  override def handle[E[_], S, R[_], Rest[_], A, B](p: Prog[R, A], s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, stateful(s0, h))

  def run[A](p: Prog[Pure, A]): A = Cek.run(p, Spots.escape)

  // -- THIS INTERFACE'S PLACES, FOR THE MACHINE

  /**
   * The place of `E` in `R`, `R` without it `Rest`, typed by this interface's rows: what okay-scala's `Place` is to
   * its own. The matches are `@unchecked` as there: an effect is never a row.
   */
  enum Spot[E[_], R[_], Rest[_]] extends At[E, R, Rest]:
    /** a handler's place meets every operation of its effect: the proof it is the handler's made once, kept here — in
     * the padding of an object of no other field, so no place is bigger for it */
    case Only[E[_]](private[cek] var proof: Taken[E, E, Void] | Null = null) extends Spot[E, E, Void]
    case Last[R[_], E[_]](private[cek] var proof: Taken[E, E, R] | Null = null) extends Spot[E, R + E, R]
    case Init[E[_], R[_], G[_], Rest[_]](at: Spot[E, R, Rest]) extends Spot[E, R + G, Rest + G]
    case Absurd[R[_]]() extends Spot[Void, R, R]

    def take[E2[_]](at: At[E2, R, ?]): Taken[E, E2, Rest] = Spots.take(this, at)
    override def outside[E2[_]](at: At[E2, R, ?]): At[E2, Rest, ?] | Null = Spots.outside(this, at)
