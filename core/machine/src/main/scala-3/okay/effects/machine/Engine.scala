package okay.effects.machine

import okay.effects.{AbortClauses, Cell, Clauses, Runner, StatefulClauses, StatefulTailClauses, Void, TailClauses}
import okay.effects.Places
import okay.effects.Places.{+, Place, Renaming, Taken, take, toEnd}
import scala.annotation.tailrec

/**
 * SCALA 3'S MACHINE, its own: its data GADTs, each an `enum` — the places, the program, the frames, the levels, the
 * handlers installed, the levels a `k` puts back — typed by Scala 3's GADT inference throughout, no cast. A program is
 * `Prog[R, A]`, of the value `A` at the row `R`, a type constructor; an operation `Perf(at, op)`, its place a typed
 * path — `Place.Last` the last effect of `R + E`, `Place.Init` one further in, `Place.Only` a row of one — the
 * evidence `Split` itself, and the operation an `E[X]`. The run is a `@tailrec` loop over the program, the frames
 * `Frames[R, X, V]` (the binds from `X` to the value `V` of the nearest level's body, aligned) and the levels
 * `Meta[R, V, Z]`, indexed by the row the program inside them is at: a handler of `E` at its place in `R`, its
 * outside at `Rest`, `R` without `E`; a remapping of places (`split`, `embed`) from `R1` to `R2`.
 *
 * An operation's path is matched against the levels: a handler's level holds its own place, and `take` of the two
 * says, by the types, that the operation is that handler's (`Taken.Mine`), or gives its place outside
 * (`Taken.Theirs`) — so a handler takes its effect from wherever it is in the row, the program not renamed. Answered in
 * place, the answer goes straight into the next frame. Taken by a general handler,
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
    /** a handler of `E` installed over `p`, taking `E`'s operations from its place `at` in `R`: a level of `lv`'s
     * shape, its state made afresh each time */
    case Handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], at: Place[E, R, Rest], lv: Level[E, Rest, A, B])
      extends Prog[Rest, B]
    case Remap[R1[_], R2[_], A](p: Prog[R1, A], rename: Renaming[R1, R2]) extends Prog[R2, A]
    /** `k` resumed with `x`: the frames inside, the levels crossed, the general handler's level put back here */
    case Resume[Rest[_], X, B, R0[_], V0, E[_], R[_], A](inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, A],
      at: Place[E, R, Rest], lv: Level[E, Rest, A, B], x: X) extends Prog[Rest, B]

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
    /** a handler of `E`'s level, at its place `at` in `R`: the program inside at `R`, its body's value `A`; the
     * frames outside it from `B` to `W` at `Rest`, the levels outside them */
    case Handler[E[_], R[_], Rest[_], A, B, W, Z](at: Place[E, R, Rest], lv: Level[E, Rest, A, B],
      outside: Frames[Rest, B, W], m: Meta[Rest, W, Z]) extends Meta[R, A, Z]
    /** a remapping's level: the program inside at `R1`, its places renamed to `R2`'s outside it */
    case Remapped[R1[_], R2[_], A, W, Z](rename: Renaming[R1, R2], outside: Frames[R2, A, W], m: Meta[R2, W, Z])
      extends Meta[R1, A, Z]

  /** levels taken off by a `k`, the innermost's body at `RIn` of `VIn`, the outermost's at `ROut` of `VOut`: data,
   * put back on any levels by `put` */
  enum Prefix[RIn[_], VIn, ROut[_], VOut]:
    case Empty[R[_], V]() extends Prefix[R, V, R, V]
    case Handler[RIn[_], VIn, E[_], R[_], Rest[_], A, B, W](inner: Prefix[RIn, VIn, R, A], at: Place[E, R, Rest],
      lv: Level[E, Rest, A, B], outside: Frames[Rest, B, W]) extends Prefix[RIn, VIn, Rest, W]
    case Remapped[RIn[_], VIn, R1[_], R2[_], A, W](inner: Prefix[RIn, VIn, R1, A], rename: Renaming[R1, R2],
      outside: Frames[R2, A, W]) extends Prefix[RIn, VIn, R2, W]

  /** the levels of `pre` put back on `m`: each a level of its own, a stateful one from the state it was taken with */
  @tailrec def put[RIn[_], VIn, ROut[_], VOut, Z](pre: Prefix[RIn, VIn, ROut, VOut], m: Meta[ROut, VOut, Z])
    : Meta[RIn, VIn, Z] =
    pre match
      case _: Prefix.Empty[r, v] => m
      case h: Prefix.Handler[ri, vi, e, r, rest, a, b, w] => put(h.inner, Meta.Handler(h.at, fresh(h.lv), h.outside, m))
      case r: Prefix.Remapped[ri, vi, r1, r2, a, w] => put(r.inner, Meta.Remapped(r.rename, r.outside, m))

  /** where the run goes on, after a cold step */
  final case class Go[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z])

  // -- THE LOOP

  /** the handler `at` is answered by, walked by the path; `null` past every handler or at the empty effect's place —
   * the operation of `Void`, which `walk` finds absurd */
  @tailrec private def found[E[_], R[_], V, Z](at: Place[E, R, ?], m: Meta[R, V, Z]): Level[E, ?, ?, ?] | Null =
    m match
      // a handler at the last place, nearly every one: the two places matched here, nothing made; another, `take`
      case l: Meta.Handler[e, r, rest, a, b, w, z] => (l.at: @unchecked) match
          case _: Place.Last[r1, e1] => (at: @unchecked) match
              case _: Place.Last[r2, e2] => l.lv
              case ip: Place.Init[e2, r2, g, rest2] => found[E, rest, w, Z](ip.s, l.m)
              case _: Place.Absurd[r2] => null
          case _ => take(l.at, at) match
              case _: Taken.Mine[e2, r2] => l.lv
              case t: Taken.Theirs[e2, e3, r2] => found[E, rest, w, Z](t.at, l.m)
      case r: Meta.Remapped[r1, r2, a, w, z] => found[E, r2, w, Z](r.rename(at), r.m)
      case _: Meta.Top[z] => null

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
    case p: Prog.Perf[r, e, x] => found[e, R, V, Z](p.at, m) match
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
    // `k` resumed: the levels crossed and the handler put back, the value straight into the frame inside — no step
    // record, no `Done`
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] =>
      val m2 = put(r.pre, Meta.Handler[e, rr, R, a, b, V, Z](r.at, r.lv, fs, m))
      r.inner match
        case t: Frames.Then[r2, x2, y, v] => loop[r0, y, v0, Z](t.f(r.x), t.rest, m2)
        case _ => loop[r0, x, v0, Z](Prog.Done(r.x), r.inner, m2)
    case _ =>
      val g = cold[R, X, V, Z](c, fs, m)
      loop(g.c, g.fs, g.m)

  /** the loop's cold steps: a level left, a level entered (a `k` resumed is the loop's own) */
  private def cold[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Go[?, ?, ?, Z] = c match
    case d: Prog.Done[r, x] => fs match
        case t: Frames.Then[r2, a, y, v] => Go[R, y, V, Z](t.f(d.a), t.rest, m)
        case t: Frames.Mapped[r2, a, y, v] => Go[R, y, V, Z](Prog.Done(t.f(d.a)), t.rest, m)
        case _: Frames.End[r2, v] => m match
            case l: Meta.Handler[e, r, rest, a, b, w, z] => Go[rest, b, w, Z](ret(l.lv, d.a), l.outside, l.m)
            case l: Meta.Remapped[r1, r2, a, w, z] => Go[r2, a, w, Z](Prog.Done(d.a), l.outside, l.m)
            // the run's end: the loop's, given back to it as it is
            case _: Meta.Top[z] => Go[R, X, V, Z](c, fs, m)
    case h: Prog.Handle[e, r, rest, a, b] =>
      Go[r, a, a, Z](h.p, Frames.End(), Meta.Handler[e, r, R, a, b, V, Z](h.at, fresh(h.lv), fs, m))
    case r: Prog.Remap[r1, r2, a] => Go[r1, a, a, Z](r.p, Frames.End(), Meta.Remapped[r1, R, a, V, Z](r.rename, fs, m))
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] =>
      Go[r0, x, v0, Z](Prog.Done(r.x), r.inner, put(r.pre, Meta.Handler[e, rr, R, a, b, V, Z](r.at, r.lv, fs, m)))
    case p: Prog.Perf[r, e, x] => walk[e, R, x, V, R, V, Z](p.at, p.op, fs, Prefix.Empty[R, V](), m)
    case b: Prog.Bind[r, y, x] => Go[R, y, V, Z](b.p, Frames.Then[R, y, x, V](b.f, fs), m)
    case b: Prog.Mapping[r, y, x] => Go[R, y, V, Z](b.p, Frames.Mapped[R, y, x, V](b.f, fs), m)
    case d: Prog.Delay[r, x] => Go[R, X, V, Z](d.p(), fs, m)

  /** an operation to its general handler: the levels it crosses taken onto `pre`, `k` made, the clause run outside */
  @tailrec private def walk[E[_], R0[_], X, V0, R[_], V, Z](at: Place[E, R, ?], op: E[X], inner: Frames[R0, X, V0],
    pre: Prefix[R0, V0, R, V], m: Meta[R, V, Z]): Go[?, ?, ?, Z] = m match
    // a handler at the last place, nearly every one: the two places matched here, nothing made; another, `take`
    case l: Meta.Handler[e, r, rest, a, b, w, z] => (l.at: @unchecked) match
        case _: Place.Last[r1, e1] => (at: @unchecked) match
            case _: Place.Last[r2, e2] => here[E, R0, X, V0, R, rest, a, b, w, Z](l, op, inner, pre)
            case ip: Place.Init[e2, r2, g, rest2] =>
              walk[E, R0, X, V0, rest, w, Z](ip.s, op, inner, Prefix.Handler(pre, l.at, fresh(l.lv), l.outside), l.m)
            case _: Place.Absurd[r2] => op.absurd
        case _ => take(l.at, at) match
            case _: Taken.Mine[e2, r2] => here[E, R0, X, V0, R, rest, a, b, w, Z](l, op, inner, pre)
            case t: Taken.Theirs[e2, e3, r2] =>
              walk[E, R0, X, V0, rest, w, Z](t.at, op, inner, Prefix.Handler(pre, l.at, fresh(l.lv), l.outside), l.m)
    case r: Meta.Remapped[r1, r2, a, w, z] =>
      walk[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, Prefix.Remapped(pre, r.rename, r.outside), r.m)
    case _: Meta.Top[z] => (at: @unchecked) match
        case _: Place.Only[e2] => op.absurd
        case _: Place.Absurd[r] => op.absurd

  /** an operation at its own handler's level `l`: the clause run outside it, `k` made — or none, aborting — or
   * answered in place, on where the operation is with the levels crossed put back */
  private def here[E[_], R0[_], X, V0, R[_], Rest[_], A, B, W, Z](l: Meta.Handler[E, R, Rest, A, B, W, Z], op: E[X],
    inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, A]): Go[?, ?, ?, Z] = l.lv match
    case g: Level.General[e3, t3, a3, b3] =>
      Go[Rest, B, W, Z](g.h.op[X](op, (x: X) => Prog.Resume[Rest, X, B, R0, V0, E, R, A](inner, pre, l.at, g, x)),
        l.outside, l.m)
    case g: Level.StateK[e3, t3, a3, b3, s] =>
      // the level reused when resumed with the state it has: no allocation per operation for a state that stays
      val k = (x: X, s1: s) => Prog.Resume[Rest, X, B, R0, V0, E, R, A](inner, pre, l.at,
        if (s1.asInstanceOf[AnyRef] eq g.state.asInstanceOf[AnyRef]) g else Level.StateK(g.h, s1), x)
      Go[Rest, B, W, Z](g.h.op[X](g.state, op, k), l.outside, l.m)
    case g: Level.Abort[e3, t3, a3, b3] =>
      // the rest dropped: the frames inside and the levels crossed left behind, no `k`
      Go[Rest, B, W, Z](g.h.abort[X](op), l.outside, l.m)
    case t: Level.Tail[e3, t3, a3, b3] => Go[R0, X, V0, Z](Prog.Done(t.h.answer[X](op)), inner, put(pre, l))
    case s: Level.State[e3, t3, a3, b3, st] =>
      Go[R0, X, V0, Z](Prog.Done(s.h.answer[X](s.cell, op)), inner, put(pre, l))

  inline def run[A](p: Prog[PureRow, A]): A = loop[PureRow, A, A, A](p, Frames.End(), Meta.Top())

/** the interface over the machine: its types the object's, a handler kept as it is given */
private[machine] class Engine extends Runner:
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
  protected[effects] def interposed[E[_], R[_], A](p: Prog[R, A])(implicit in: In[E, R]): Prog[R + E, A] =
    Prog.Remap(p, Places.interposing(in))

  def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), Level.General(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), Level.Tail(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), Level.Abort(h))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B])
    : Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), Level.State(h, Cell.Var(s0)))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulHandler[E, S, R, A, B])
    : Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), Level.StateK(h, s0))

  // a handler takes its effect from its place at once: no `split`, no program renamed
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: Handler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, Level.General(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: TailHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, Level.Tail(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: AbortHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, Level.Abort(h))
  override def handle[E[_], S, R[_], Rest[_], A, B](p: Prog[R, A], s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, Level.State(h, Cell.Var(s0)))
  override def handle[E[_], S, R[_], Rest[_], A, B](p: Prog[R, A], s0: S, h: StatefulHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, Level.StateK(h, s0))
  def run[A](p: Prog[PureRow, A]): A = Engine.run(p)
