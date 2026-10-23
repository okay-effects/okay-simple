package okay.effects.cek

import okay.effects.{AbortClauses, Clauses, Places, Runner, Self, StatefulTailClauses, TailClauses, Void}
import okay.effects.Places.{Place, Renaming}
import okay.scala3.{At, Cek, Escape, Handler as Machines, Prog, Sub as Renamed, Taken as Took}

/**
 * THE INTERFACE ON OKAY-SCALA'S MACHINE (github.com/okay-effects/okay-scala), a CEK machine: a `Runner` whose programs
 * are okay-scala's `Prog`, over this interface's own rows — `import okay.effects.cek.OnCek.*`, and a program written
 * against the interface runs there as it is. Neither library depends on the other's classes: okay-scala's machine
 * takes places of any representation (its `At`), and this adapter gives it this interface's own, `Places.Place`, each
 * compared by `Places.take`, whose `Mine` proves — by this interface's own types — that an operation is the handler's.
 */
object OnCek extends Runner with Self:
  type ![+A, R[_]] = Prog[R, A]
  type In[E[_], R[_]] = Place[E, R, ?]
  type Split[E[_], R[_], Rest[_]] = Place[E, R, Rest]
  type Sub[R[_], S[_]] = Renaming[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = Place.Only[E]()
  implicit def splitLast[R[_], E[_]]: Split[E, R + E, R] = Place.Last[R, E]()
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Place.Init[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Places.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Places.subOne(e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Places.subMore(r, e)

  def pure[A, R[_]](a: A): Prog[R, A] = Prog.Value(a)
  def flatMap[A, B, R[_]](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Prog.FlatMap(p, f)
  override def map[A, B, R[_]](p: Prog[R, A])(f: A => B): Prog[R, B] = Prog.Map(p, f)
  def delay[A, R[_]](p: => Prog[R, A]): Prog[R, A] = Prog.Defer(() => p)
  override protected[effects] def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): Prog[R, X] =
    Prog.Perform[R, E, X](Here(in), op)

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Rest + E, A] =
    (s: @unchecked) match
      case _: Place.Last[r, e] => p
      case _ => Prog.Remap(p, renamed(Places.toEnd(s)))
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Prog.Remap(p, renamed(s))
  override protected[effects] def interposed[E[_], R[_], A](p: Prog[R, A])(implicit in: In[E, R]): Prog[R + E, A] =
    Prog.Remap(p, renamed(Places.interposing(in)))

  // this interface's handlers, as the machine's own of the same shape

  private def general[E[_], R[_], A, B](h: Clauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.General[E, R, A, B](a => h.ret(a), [X] => (op: E[X], k: X => Prog[R, B]) => h.op(op, k))
  private def tail[E[_], R[_], A, B](h: TailClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Tail[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.answer(op))
  private def abort[E[_], R[_], A, B](h: AbortClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Abort[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.abort(op))
  /** the machine's cell is its own: the clause sees it as this interface's `Cell` */
  private def stateful[E[_], S, R[_], A, B](s0: S, h: StatefulTailClauses[E, S, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.stateful[E, R, A, B, S](s0)((s, a) => h.ret(s, a))(
      [X] => (op: E[X], cell: okay.scala3.Cell[S]) => h.answer(new Cell[S] {
        def get: S = cell.state
        def set(s: S): Unit = cell.state = s
      }, op))

  def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Here(Place.Last[R, E]()), general(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Here(Place.Last[R, E]()), tail(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Here(Place.Last[R, E]()), abort(h))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B])
    : Prog[R, B] = Prog.Handle(p, Here(Place.Last[R, E]()), stateful(s0, h))

  // a handler takes its effect from its place at once: no `split`
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: Handler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, Here(s), general(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: TailHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, Here(s), tail(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: AbortHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, Here(s), abort(h))
  override def handle[E[_], S, R[_], Rest[_], A, B](p: Prog[R, A], s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, Here(s), stateful(s0, h))

  def run[A](p: Prog[Pure, A]): A = Cek.run(p, escape)

  // -- THIS INTERFACE'S PLACES, AS THE MACHINE TAKES THEM

  /** a place of this interface, for the machine: compared with another by `Places.take` */
  final class Here[E[_], R[_], Rest[_]](val place: Place[E, R, Rest]) extends At[E, R, Rest]:
    def take[E2[_]](at: At[E2, R, ?]): Took[E, E2, Rest] = at match
      case h: Here[E2, R, rest2] @unchecked => Places.take(place, h.place) match
          case _: Places.Taken.Mine[e, r] => Took.Mine()
          case t: Places.Taken.Theirs[e, e2, r] => Took.Theirs(here(t.at))
      case _ => throw IllegalStateException(s"a place of another representation in this interface's row: $at")

  private def here[E[_], R[_]](at: Place[E, R, ?]): At[E, R, ?] = at match
    case p: Place[E, R, rest] @unchecked => Here(p)

  /** this interface's renaming of places, for the machine */
  private def renamed[R1[_], R2[_]](r: Renaming[R1, R2]): Renamed[R1, R2] = new Renamed[R1, R2]:
    def apply[E[_]](at: At[E, R1, ?]): At[E, R2, ?] = at match
      case h: Here[E, R1, rest] @unchecked => here(r(h.place))
      case _ => throw IllegalStateException(s"a place of another representation in this interface's row: $at")

  /** past every handler, an operation is at `Pure`'s one place or the empty effect's: of `Void`, absurd */
  private val escape: Escape[Pure] = new Escape[Pure]:
    def apply[E[_], X](at: At[E, Pure, ?], op: E[X]): Nothing = at match
      case h: Here[E, Pure, rest] @unchecked => (h.place: @unchecked) match
          case _: Place.Only[e] => op.absurd
          case _: Place.Absurd[r] => op.absurd
      case _ => throw IllegalStateException(s"a place of another representation in this interface's row: $at")
