package okay.effects.machine

import okay.effects.{AbortClauses, Cell, Clauses, Effects, StatefulClauses, StatefulTailClauses, Void, TailClauses}
import okay.effects.Places
import okay.effects.Places.{+, Place, Renaming, toEnd}
import scala.annotation.tailrec

/**
 * SCALA 3'S MACHINE, its own: its data GADTs, each an `enum` — the places, the program, the frames, the levels, the
 * handlers installed, the levels a `k` puts back — typed by Scala 3's GADT inference throughout, no cast. A program is
 * `Prog[R, A]`, of the value `A` at the row `R`, a type constructor; an operation `Perf(at, op)`, its place a typed
 * path — `Place.Last` the last effect of `R + E`, `Place.Init` one further in, `Place.Only` a row of one — the
 * evidence `Split` itself, and the operation an `E[X]`. The run is a `@tailrec` loop over the program, the frames
 * `Frames[R, X, V]` (the binds from `X` to the value `V` of the nearest level's body, aligned) and the levels
 * `Meta[R, V, Z]`, indexed by the row the program inside them is at: a handler of `E` at `T + E`, its outside at `T`;
 * a remapping of places (`split`, `embed`) from `R1` to `R2`.
 *
 * An operation's path is matched against the levels: `Place.Last` at a handler's level is, by the types, that
 * handler's operation. Answered in place, the answer goes straight into the next frame. Taken by a general handler,
 * the levels crossed become a `Prefix`, data, put back where `k` is resumed — a stateful level with the state it had
 * when `k` was taken. `k` resumed is a program, `Prog.Resume`. The loop holds the hot cases; a level entered or a
 * `k` resumed is `cold`'s.
 */
/** the machine's data and its loop: in an object, static — no outer pointer in a node or a level, no outer test in
 * a match */
private[machine] object Engine:
  type PureRow[X] = Void[X]
  /** a program at `R` at `Rest + E`: itself if `R` is that already, else remapped */
  inline def moved[E[_], R[_], Rest[_], A](at: Place[E, R, Rest], p: Prog[R, A]): Prog[Rest + E, A] = at match
    case _: Place.Last[r, e] => p
    case _ => Prog.Remap(p, toEnd(at))

  // -- PROGRAMS

  enum Prog[R[_], +A]:
    case Done[R[_], A](a: A) extends Prog[R, A]
    case Bind[R[_], X, A](p: Prog[R, X], f: X => Prog[R, A]) extends Prog[R, A]
    /** a value through `f`: one step, no program made of it */
    case Mapping[R[_], X, A](p: Prog[R, X], f: X => A) extends Prog[R, A]
    case Delay[R[_], A](p: () => Prog[R, A]) extends Prog[R, A]
    case Perf[R[_], E[_], X](at: Place[E, R, ?], op: E[X]) extends Prog[R, X]
    /** a handler of `E` installed over `p`: a level of `lv`'s shape, its state made afresh each time */
    case Handle[E[_], T[_], A, B](p: Prog[T + E, A], lv: Level[E, T, A, B]) extends Prog[T, B]
    case Remap[R1[_], R2[_], A](p: Prog[R1, A], rename: Renaming[R1, R2]) extends Prog[R2, A]
    /** `k` resumed with `x`: the frames inside, the levels crossed, the general handler's level put back here */
    case Resume[T[_], X, B, R0[_], V0, E[_], A](inner: Frames[R0, X, V0], pre: Prefix[R0, V0, T + E, A],
        lv: Level[E, T, A, B], x: X) extends Prog[T, B]

  // -- THE FRAMES: from a value `X` to the value `V` of the nearest level's body, at the row `R`

  enum Frames[R[_], -X, +V]:
    case End[R[_], V]() extends Frames[R, V, V]
    case Then[R[_], X, Y, V](f: X => Prog[R, Y], rest: Frames[R, Y, V]) extends Frames[R, X, V]
    /** a map pending: the value through `f`, on into `rest` */
    case Mapped[R[_], X, Y, V](f: X => Y, rest: Frames[R, Y, V]) extends Frames[R, X, V]

  // -- THE HANDLERS INSTALLED: of `E`, the row outside `T`, the body's value `A`, the handler's `B`

  enum Level[E[_], T[_], A, B]:
    case General[E[_], T[_], A, B](h: Clauses[E, A, Prog[T, B]]) extends Level[E, T, A, B]
    /** a general handler with a state of its own: the state of its time, kept as it is */
    case StateK[E[_], T[_], A, B, S](h: StatefulClauses[E, S, A, Prog[T, B]], state: S) extends Level[E, T, A, B]
    /** never resuming `k`: an operation found here leaves at once with the clause's result, no `k` made */
    case Abort[E[_], T[_], A, B](h: AbortClauses[E, A, Prog[T, B]]) extends Level[E, T, A, B]
    /** answering in place */
    case Tail[E[_], T[_], A, B](h: TailClauses[E, A, Prog[T, B]]) extends Level[E, T, A, B]
    /** answering in place, its state in `cell`: of its level, made afresh each time it is installed */
    case State[E[_], T[_], A, B, S](h: StatefulTailClauses[E, S, A, Prog[T, B]], cell: Cell.Var[S])
        extends Level[E, T, A, B]

  /** the level as it is to be installed, or put back by a `k`: a stateful one with a cell of its own, from the
   * state it has now */
  inline def fresh[E[_], T[_], A, B](lv: Level[E, T, A, B]): Level[E, T, A, B] = lv match
    case s: Level.State[e, t, a, b, st] => Level.State(s.h, Cell.Var(s.cell.state))
    case _ => lv

  /** the body's value given back through the handler */
  inline def ret[E[_], T[_], A, B](lv: Level[E, T, A, B], a: A): Prog[T, B] = lv match
    case g: Level.General[e, t, a2, b] => g.h.ret(a)
    case s: Level.StateK[e, t, a2, b, st] => s.h.ret(s.state, a)
    case t: Level.Tail[e, t2, a2, b] => t.h.ret(a)
    case s: Level.State[e, t, a2, b, st] => s.h.ret(s.cell.state, a)
    case x: Level.Abort[e, t, a2, b] => x.h.ret(a)

  // -- THE LEVELS: the program inside at `R`, its body's value `V`, the run's `Z`

  enum Meta[R[_], V, Z]:
    case Top[Z]() extends Meta[PureRow, Z, Z]
    /** a handler of `E`'s level: the program inside at `T + E`, its body's value `A`; the frames outside it from `B`
     * to `W` at `T`, the levels outside them */
    case Handler[E[_], T[_], A, B, W, Z](lv: Level[E, T, A, B], outside: Frames[T, B, W], m: Meta[T, W, Z])
        extends Meta[T + E, A, Z]
    /** a remapping's level: the program inside at `R1`, its places renamed to `R2`'s outside it */
    case Remapped[R1[_], R2[_], A, W, Z](rename: Renaming[R1, R2], outside: Frames[R2, A, W], m: Meta[R2, W, Z])
        extends Meta[R1, A, Z]

  /** levels taken off by a `k`, the innermost's body at `RIn` of `VIn`, the outermost's at `ROut` of `VOut`: data,
   * put back on any levels by `put` */
  enum Prefix[RIn[_], VIn, ROut[_], VOut]:
    case Empty[R[_], V]() extends Prefix[R, V, R, V]
    case Handler[RIn[_], VIn, E[_], T[_], A, B, W](inner: Prefix[RIn, VIn, T + E, A], lv: Level[E, T, A, B],
        outside: Frames[T, B, W]) extends Prefix[RIn, VIn, T, W]
    case Remapped[RIn[_], VIn, R1[_], R2[_], A, W](inner: Prefix[RIn, VIn, R1, A], rename: Renaming[R1, R2],
        outside: Frames[R2, A, W]) extends Prefix[RIn, VIn, R2, W]

  /** the levels of `pre` put back on `m`: each a level of its own, a stateful one from the state it was taken with */
  @tailrec def put[RIn[_], VIn, ROut[_], VOut, Z](pre: Prefix[RIn, VIn, ROut, VOut], m: Meta[ROut, VOut, Z]): Meta[RIn, VIn, Z] =
    pre match
      case _: Prefix.Empty[r, v] => m
      case h: Prefix.Handler[ri, vi, e, t, a, b, w] => put(h.inner, Meta.Handler(fresh(h.lv), h.outside, m))
      case r: Prefix.Remapped[ri, vi, r1, r2, a, w] => put(r.inner, Meta.Remapped(r.rename, r.outside, m))

  /** where the run goes on, after a cold step */
  final case class Go[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z])

  // -- THE LOOP

  /** the handler `at` is answered by, walked by the path. An operation at the empty effect's place, or past every
   * handler — in the row of no effect, where its only place is `Only[Void]` — is an operation of `Void`: there is
   * none, absurd */
  @tailrec private def found[E[_], X, R[_], V, Z](at: Place[E, R, ?], op: E[X], m: Meta[R, V, Z]): Level[E, ?, ?, ?] =
    m match
      case l: Meta.Handler[e, t, a, b, w, z] => (at: @unchecked) match
        case _: Place.Last[r, e2] => l.lv
        case ip: Place.Init[e2, r, g, rest] => found[E, X, t, w, Z](ip.s, op, l.m)
        case _: Place.Absurd[r] => op.absurd
      case r: Meta.Remapped[r1, r2, a, w, z] => found[E, X, r2, w, Z](r.rename(at), op, r.m)
      case _: Meta.Top[z] => (at: @unchecked) match
        case _: Place.Only[e2] => op.absurd
        case _: Place.Absurd[r] => op.absurd

  @tailrec private def loop[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Z = c match
    case d: Prog.Done[r, x] => fs match
      case t: Frames.Then[r2, a, y, v] => loop[R, y, V, Z](t.f(d.a), t.rest, m)
      case t: Frames.Mapped[r2, a, y, v] => loop[R, y, V, Z](Prog.Done(t.f(d.a)), t.rest, m)
      case _: Frames.End[r2, v] => m match
        case _: Meta.Top[z] => d.a
        case _ =>
          val g = cold[R, X, V, Z](c, fs, m)
          loop(g.c, g.fs, g.m)
    case b: Prog.Bind[r, y, x] => loop[R, y, V, Z](b.p, Frames.Then[R, y, x, V](b.f, fs), m)
    case b: Prog.Mapping[r, y, x] => loop[R, y, V, Z](b.p, Frames.Mapped[R, y, x, V](b.f, fs), m)
    case p: Prog.Perf[r, e, x] => found[e, x, R, V, Z](p.at, p.op, m) match
      // answered in place, the answer straight into the next frame: no `Done` made
      case t: Level.Tail[e2, t2, a2, b2] =>
        val v = t.h.answer[x](p.op)
        fs match
          case f: Frames.Then[r2, a3, y, w] => loop[R, y, V, Z](f.f(v), f.rest, m)
          case _ => loop[R, X, V, Z](Prog.Done(v), fs, m)
      case s: Level.State[e2, t2, a2, b2, st] =>
        val v = s.h.answer[x](s.cell, p.op)
        fs match
          case f: Frames.Then[r2, a3, y, w] => loop[R, y, V, Z](f.f(v), f.rest, m)
          case _ => loop[R, X, V, Z](Prog.Done(v), fs, m)
      case _ =>
        val g = walk[e, R, x, V, R, V, Z](p.at, p.op, fs, Prefix.Empty[R, V](), m)
        loop(g.c, g.fs, g.m)
    case d: Prog.Delay[r, x] => loop[R, X, V, Z](d.p(), fs, m)
    case _ =>
      val g = cold[R, X, V, Z](c, fs, m)
      loop(g.c, g.fs, g.m)

  /** the loop's cold steps: a level left, a level entered, a `k` resumed */
  private def cold[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Go[?, ?, ?, Z] = c match
    case d: Prog.Done[r, x] => fs match
      case t: Frames.Then[r2, a, y, v] => Go[R, y, V, Z](t.f(d.a), t.rest, m)
      case t: Frames.Mapped[r2, a, y, v] => Go[R, y, V, Z](Prog.Done(t.f(d.a)), t.rest, m)
      case _: Frames.End[r2, v] => m match
        case l: Meta.Handler[e, t, a, b, w, z] => Go[t, b, w, Z](ret(l.lv, d.a), l.outside, l.m)
        case l: Meta.Remapped[r1, r2, a, w, z] => Go[r2, a, w, Z](Prog.Done(d.a), l.outside, l.m)
        // the run's end: the loop's, given back to it as it is
        case _: Meta.Top[z] => Go[R, X, V, Z](c, fs, m)
    case h: Prog.Handle[e, t, a, b] =>
      Go[R + e, a, a, Z](h.p, Frames.End(), Meta.Handler[e, R, a, b, V, Z](fresh(h.lv), fs, m))
    case r: Prog.Remap[r1, r2, a] => Go[r1, a, a, Z](r.p, Frames.End(), Meta.Remapped[r1, R, a, V, Z](r.rename, fs, m))
    case r: Prog.Resume[t, x, b, r0, v0, e, a] =>
      Go[r0, x, v0, Z](Prog.Done(r.x), r.inner, put(r.pre, Meta.Handler[e, R, a, b, V, Z](r.lv, fs, m)))
    case p: Prog.Perf[r, e, x] => walk[e, R, x, V, R, V, Z](p.at, p.op, fs, Prefix.Empty[R, V](), m)
    case b: Prog.Bind[r, y, x] => Go[R, y, V, Z](b.p, Frames.Then[R, y, x, V](b.f, fs), m)
    case b: Prog.Mapping[r, y, x] => Go[R, y, V, Z](b.p, Frames.Mapped[R, y, x, V](b.f, fs), m)
    case d: Prog.Delay[r, x] => Go[R, X, V, Z](d.p(), fs, m)

  /** an operation to its general handler: the levels it crosses taken onto `pre`, `k` made, the clause run outside */
  @tailrec private def walk[E[_], R0[_], X, V0, R[_], V, Z](at: Place[E, R, ?], op: E[X], inner: Frames[R0, X, V0],
      pre: Prefix[R0, V0, R, V], m: Meta[R, V, Z]): Go[?, ?, ?, Z] = m match
    case l: Meta.Handler[e, t, a, b, w, z] => (at: @unchecked) match
      case _: Place.Last[r, e2] => l.lv match
        case g: Level.General[e3, t3, a3, b3] =>
          Go[t, b, w, Z](g.h.op[X](op, (x: X) => Prog.Resume[t, X, b, R0, V0, E, a](inner, pre, g, x)), l.outside, l.m)
        case g: Level.StateK[e3, t3, a3, b3, s] =>
          val k = (x: X, s1: s) => Prog.Resume[t, X, b, R0, V0, E, a](inner, pre, Level.StateK(g.h, s1), x)
          Go[t, b, w, Z](g.h.op[X](g.state, op, k), l.outside, l.m)
        case g: Level.Abort[e3, t3, a3, b3] =>
          // the rest dropped: the frames inside and the levels crossed left behind, no `k`
          Go[t, b, w, Z](g.h.abort[X](op), l.outside, l.m)
        // answered in place: on where the operation is, with the levels crossed put back
        case t: Level.Tail[e3, t3, a3, b3] => Go[R0, X, V0, Z](Prog.Done(t.h.answer[X](op)), inner, put(pre, m))
        case s: Level.State[e3, t3, a3, b3, st] =>
          Go[R0, X, V0, Z](Prog.Done(s.h.answer[X](s.cell, op)), inner, put(pre, m))
      case ip: Place.Init[e2, r, g, rest] =>
        walk[E, R0, X, V0, t, w, Z](ip.s, op, inner, Prefix.Handler(pre, fresh(l.lv), l.outside), l.m)
      case _: Place.Absurd[r] => op.absurd
    case r: Meta.Remapped[r1, r2, a, w, z] =>
      walk[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, Prefix.Remapped(pre, r.rename, r.outside), r.m)
    case _: Meta.Top[z] => (at: @unchecked) match
      case _: Place.Only[e2] => op.absurd
      case _: Place.Absurd[r] => op.absurd

  inline def run[A](p: Prog[PureRow, A]): A = loop[PureRow, A, A, A](p, Frames.End(), Meta.Top())

/** the interface over the machine: its types the object's, a handler kept as it is given */
private[machine] class Engine extends Effects:
  import Engine.*

  type ![+A, R[_]] = Prog[R, A]

  type Split[E[_], R[_], Rest[_]] = Place[E, R, Rest]
  type In[E[_], R[_]] = Place[E, R, ?]
  type Sub[R[_], S[_]] = Renaming[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = Place.Only[E]()
  implicit def splitLast[R[_], E[_]]: Split[E, R + E, R] = Place.Last[R, E]()
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Place.Init[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Places.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Places.subOne(e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Places.subMore(r, e)

  // -- THE INTERFACE

  def pure[A, R[_]](a: A): Prog[R, A] = Prog.Done(a)
  def flatMap[A, B, R[_]](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Prog.Bind(p, f)
  override def map[A, B, R[_]](p: Prog[R, A])(f: A => B): Prog[R, B] = Prog.Mapping(p, f)
  def delay[A, R[_]](p: => Prog[R, A]): Prog[R, A] = Prog.Delay(() => p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): Prog[R, X] = Prog.Perf[R, E, X](in, op)

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Rest + E, A] = moved(s, p)
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Prog.Remap(p, s)

  def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Level.General(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Level.Tail(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Level.Abort(h))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Level.State(h, Cell.Var(s0)))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulHandler[E, S, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Level.StateK(h, s0))
  def run[A](p: Prog[PureRow, A]): A = Engine.run(p)
