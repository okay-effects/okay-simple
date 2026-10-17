package okay.simple.machine

import okay.api.{Effect, Effects}
import scala.annotation.tailrec

/**
 * THE MACHINE'S TYPES, typed by the row. A program is `Prog[R, A]`, of the value `A` at the row `R`; an operation of
 * `E` is `Perf(at: At[E, R], op: Op[E, X])` — its place in the row, typed, and the operation of the type its name
 * says. The run is a typed loop over three things:
 *
 *  - the program, `Prog[R, X]`;
 *  - the FRAMES, `Frames[R, X, V]`: the binds at the row `R` from the value `X` to the value `V` of the nearest
 *    level's body — aligned, each bind's output the next one's input;
 *  - the LEVELS, `Meta[R, V, Z]`: the stack the program at the row `R` runs in, from `V` to the run's value `Z` — a
 *    handler of `E` at the row `T + E`, its outside at `T`; a remapping of places from a row to another (`split`,
 *    `embed`) — each with the frames outside it.
 *
 * What differs between the compilers is the place (`Places`, in `scala-2/` and `scala-3/`): in Scala 3 a typed path
 * matched against the levels — the handler found is the operation's own by the types, no cast; in Scala 2 a number,
 * the levels walked by it — Scala 2 cannot refine types by a match, so one cast where the operation meets its handler.
 */
private[machine] trait MachineTypes extends Effects {
  final abstract class PureRow
  final abstract class Plus[R, E]
  type Pure = PureRow
  type +[R, E <: Effect] = Plus[R, E]
  type ![+A, R] = Prog[R, A]

  /** the place of `E` in the row `R` */
  type At[E <: Effect, R]
  /** places in the row `R1` renamed to their places in `R2` */
  type Rename[R1, R2]

  sealed trait Prog[R, +A]
  final case class Done[R, A](a: A) extends Prog[R, A]
  final case class Bind[R, X, A](p: Prog[R, X], f: X => Prog[R, A]) extends Prog[R, A]
  final case class Delay[R, A](p: () => Prog[R, A]) extends Prog[R, A]
  final case class Perf[R, E <: Effect, X](at: At[E, R], op: Op[E, X]) extends Prog[R, X]
  final case class HandleGeneral[E <: Effect, R, A, B](p: Prog[Plus[R, E], A], h: Handler[E, R, A, B]) extends Prog[R, B]
  final case class HandleTail[E <: Effect, R, A, B](p: Prog[Plus[R, E], A], h: TailHandler[E, R, A, B]) extends Prog[R, B]
  final case class HandleState[E <: Effect, S, R, A, B](p: Prog[Plus[R, E], A], s0: S, h: StatefulHandler[E, S, R, A, B])
      extends Prog[R, B]
  final case class Remap[R1, R2, A](p: Prog[R1, A], rename: Rename[R1, R2]) extends Prog[R2, A]
  final case class Resume[R, X, B](k: K[R, X, B], x: X) extends Prog[R, B]

  /** the binds at the row `R` from a value `X` to the value `V` of the nearest level's body */
  sealed abstract class Frames[R, -X, +V]
  /** the end of the frames: its value the level's body's — typed by the bounds a match on it gives, `X <: V` */
  final class Last[R, V] extends Frames[R, V, V] { def last(x: V): V = x }
  final class Then[R, X, Y, V](val f: X => Prog[R, Y], val rest: Frames[R, Y, V]) extends Frames[R, X, V]

  /** a level, by its fields: its kind, the one further out (`null` at the top), a remapping's numbers — read by a walk
   * with no call */
  sealed abstract class Level0(final val kind: Int, final val outer: Level0)
  final val TopKind = 0
  final val GeneralKind = 1
  final val TailKind = 2
  final val StateKind = 3
  final val RemapKind = 4

  /** the levels the program at the row `R` runs in, from the value `V` of the nearest one's body to the run's `Z` */
  sealed abstract class Meta[R, V, Z](kind: Int, outer: Level0) extends Level0(kind, outer) {
    /** the run's value: of `Top` only */
    def top(v: V): Z
    /** the body's value given back through this level: where the run goes on outside it */
    def returned(v: V): Next[Z]
    /** this level crossed by an operation: `pre` with it after, and the levels outside it */
    def stepOut[R0, V0](pre: Prefix[R0, V0, R, V]): Out[R0, V0, Z]
    /** an operation found here by a number (Scala 2's walk): of a general handler's level only */
    def foundAny[R0, X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, V]): Next[Z] =
      throw new IllegalStateException("not a handler")
  }
  final class Top[Z] extends Meta[PureRow, Z, Z](TopKind, null) {
    def top(v: Z): Z = v
    def returned(v: Z): Next[Z] = throw new IllegalStateException("the top is the loop's")
    def stepOut[R0, V0](pre: Prefix[R0, V0, PureRow, Z]): Out[R0, V0, Z] = throw new IllegalStateException("no handler")
  }

  /** a handler's level, of `E`: what the walk to an operation's handler finds, typed by the effect */
  sealed trait Handling[E <: Effect] {
    def kind: Int
    /** an operation answered in place: of a handler answering in place, or a stateful one */
    def answer[X](op: Op[E, X]): X
  }
  /** a handler of `E`'s level, the program inside at `T + E`, its body's value `A`; the frames outside it from `B`
   * to `W` at `T`, the levels outside them */
  sealed abstract class HandlerLevel[E <: Effect, T, A, B, W, Z](kind: Int, final val outside: Frames[T, B, W],
      final val m: Meta[T, W, Z]) extends Meta[Plus[T, E], A, Z](kind, m) with Handling[E] {
    def top(v: A): Z = throw new IllegalStateException("not the top")
    /** `pre` with this level after it, as it is now */
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W]
    def stepOut[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Out[R0, V0, Z] = out(crossed(pre), m)
    /** the operation's `k` taken, its clause run: of a general handler only */
    def found[R0, X, V0](op: Op[E, X], inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E], A]): Next[Z] =
      throw new IllegalStateException("answered in place")
    override def foundAny[R0, X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E], A]): Next[Z] =
      found[R0, X, V0](opAs[E, X](op), inner, pre)
  }
  final class GeneralLevel[E <: Effect, T, A, B, W, Z](val h: Handler[E, T, A, B], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](GeneralKind, outside, m) {
    def returned(v: A): Next[Z] = next[T, B, W, Z](h.ret(v), outside, m)
    def answer[X](op: Op[E, X]): X = throw new IllegalStateException("not answered in place")
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new GeneralLevel(h, outside, m2))
    }
    override def found[R0, X, V0](op: Op[E, X], inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E], A]): Next[Z] = {
      val k = new K[T, X, B] {
        def at[W2, Z2](x: X, fs: Frames[T, B, W2], m2: Meta[T, W2, Z2]): Next[Z2] =
          next[R0, X, V0, Z2](Done(x), inner, pre.put(new GeneralLevel(h, fs, m2)))
      }
      next[T, B, W, Z](h.op[X](op, (x: X) => Resume(k, x)), outside, m)
    }
  }
  final class TailLevel[E <: Effect, T, A, B, W, Z](val h: TailHandler[E, T, A, B], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](TailKind, outside, m) {
    def returned(v: A): Next[Z] = next[T, B, W, Z](h.ret(v), outside, m)
    def answer[X](op: Op[E, X]): X = h.answer[X](op)
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new TailLevel(h, outside, m2))
    }
  }
  /** a stateful handler's level: its state the cell its clauses write */
  final class StateLevel[E <: Effect, S, T, A, B, W, Z](val h: StatefulHandler[E, S, T, A, B], var state: S,
      outside: Frames[T, B, W], m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](StateKind, outside, m) with Cell[S] {
    def returned(v: A): Next[Z] = next[T, B, W, Z](h.ret(state, v), outside, m)
    def get: S = state
    def set(s: S): Unit = state = s
    def answer[X](op: Op[E, X]): X = h.answer[X](this, op)
    /** put back with the state it has now, each time */
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] = {
      val now = state
      new Prefix[R0, V0, T, W] { def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new StateLevel(h, now, outside, m2)) }
    }
  }
  /** a remapping's level: the program inside at `R1`, its places renamed to `R2`'s outside it */
  final class RemapLevel[R1, R2, A, W, Z](val rename: Rename[R1, R2], final val outside: Frames[R2, A, W],
      final val m: Meta[R2, W, Z]) extends Meta[R1, A, Z](RemapKind, m) {
    def top(v: A): Z = throw new IllegalStateException("not the top")
    def returned(v: A): Next[Z] = next[R2, A, W, Z](Done(v), outside, m)
    def crossed[R0, V0](pre: Prefix[R0, V0, R1, A]): Prefix[R0, V0, R2, W] = new Prefix[R0, V0, R2, W] {
      def put[Z2](m2: Meta[R2, W, Z2]): Meta[R0, V0, Z2] = pre.put(new RemapLevel(rename, outside, m2))
    }
    def stepOut[R0, V0](pre: Prefix[R0, V0, R1, A]): Out[R0, V0, Z] = out(crossed(pre), m)
  }

  /** levels taken off the stack, the innermost's body at `RIn` of `VIn`, the outermost at `ROut`, its value `VOut`:
   * put back on any levels */
  trait Prefix[RIn, VIn, ROut, VOut] { def put[Z](m: Meta[ROut, VOut, Z]): Meta[RIn, VIn, Z] }
  def none[R, V]: Prefix[R, V, R, V] = new Prefix[R, V, R, V] { def put[Z](m: Meta[R, V, Z]): Meta[R, V, Z] = m }
  /** a level crossed: the prefix with it, the levels outside */
  abstract class Out[R0, V0, Z] {
    type R
    type V
    val pre: Prefix[R0, V0, R, V]
    val m: Meta[R, V, Z]
  }
  def out[R0, V0, R1, V1, Z](p: Prefix[R0, V0, R1, V1], m1: Meta[R1, V1, Z]): Out[R0, V0, Z] = new Out[R0, V0, Z] {
    type R = R1
    type V = V1
    val pre = p
    val m = m1
  }

  /** `k`, resumed at the row `R` with an `X`, where the frames from `B` and the levels are */
  trait K[R, X, B] { def at[W, Z](x: X, fs: Frames[R, B, W], m: Meta[R, W, Z]): Next[Z] }
  /** where the run goes on: a program, its frames, the levels — their types its own */
  abstract class Next[Z] {
    type R
    type X
    type V
    val c: Prog[R, X]
    val fs: Frames[R, X, V]
    val m: Meta[R, V, Z]
  }
  def next[R0, X0, V0, Z](c0: Prog[R0, X0], fs0: Frames[R0, X0, V0], m0: Meta[R0, V0, Z]): Next[Z] = new Next[Z] {
    type R = R0
    type X = X0
    type V = V0
    val c = c0
    val fs = fs0
    val m = m0
  }

  // -- the compiler's own (`Places`): the place, the evidence, and the walk to an operation's handler

  /** the place an operation is performed at, of its evidence */
  def placeOf[E <: Effect, R](in: In[E, R]): At[E, R]
  /** Scala 2's trusted step, an operation found by its number of its handler's operations; never in Scala 3's */
  def opAs[E <: Effect, X](op: Any): Op[E, X]

  /** the handler of `E` answering `at` from `m` */
  def levelOf[E <: Effect, R, V, Z](at: At[E, R], m: Meta[R, V, Z]): Handling[E]
  /** an operation to its general handler: the levels it crosses taken as `k`'s, the clause run outside them */
  def capture[E <: Effect, R, X, V, Z](at: At[E, R], op: Op[E, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Next[Z]
}

/**
 * THE MACHINE: a `@tailrec` loop of the hot cases — a bind, a value into a frame, an operation answered in place —
 * small, so the JIT compiles it whole; the rest `cold`'s. An operation answered in place goes straight into the next
 * frame, no `Done` made. A general handler's clause gets `k`: the frames inside, the levels crossed as a typed
 * `Prefix`, put back where `k` is resumed — a stateful level with the state of its time.
 */
private[machine] class Machine extends Places {
  @tailrec private def loop[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Z = c match {
    case d: Done[r, x] => fs match {
      case t: Then[r2, a, y, v] => loop[R, y, V, Z](t.f(d.a), t.rest, m)
      case _ =>
        if (m.kind == TopKind) finish[R, X, V, Z](c, fs, m)
        else {
          val n = cold[R, X, V, Z](c, fs, m)
          loop[n.R, n.X, n.V, Z](n.c, n.fs, n.m)
        }
    }
    case b: Bind[r, y, x] => loop[R, y, V, Z](b.p, new Then[R, y, x, V](b.f, fs), m)
    case p: Perf[r, e, x] =>
      val h = levelOf[e, R, V, Z](p.at, m)
      if (h.kind != GeneralKind) {
        // answered in place, the answer straight into the next frame: no `Done` made
        val x = h.answer[x](p.op)
        fs match {
          case t: Then[r2, a, y, v] => loop[R, y, V, Z](t.f(x), t.rest, m)
          case _ => loop[R, X, V, Z](Done(x), fs, m)
        }
      } else {
        val n = capture[e, R, x, V, Z](p.at, p.op, fs, m)
        loop[n.R, n.X, n.V, Z](n.c, n.fs, n.m)
      }
    case d: Delay[r, x] => loop[R, X, V, Z](d.p(), fs, m)
    case _ =>
      val n = cold[R, X, V, Z](c, fs, m)
      loop[n.R, n.X, n.V, Z](n.c, n.fs, n.m)
  }

  /** the run's end at the top: the value at the end of the frames, the run's — a frame left goes on into it */
  private def finish[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Z = c match {
    case d: Done[r, x] => fs match {
      case l: Last[r2, v] => m.top(l.last(d.a))
      case t: Then[r2, a, y, v] => loop[R, y, V, Z](t.f(d.a), t.rest, m)
    }
    case _ => loop[R, X, V, Z](c, fs, m)
  }

  /** the loop's cold steps: a level left, a level entered, a `k` resumed */
  private def cold[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Next[Z] = c match {
    case d: Done[r, x] => fs match {
      case t: Then[r2, a, y, v] => next[R, y, V, Z](t.f(d.a), t.rest, m)
      // the body's value given back through its level
      case l: Last[r2, v] => m.returned(l.last(d.a))
    }
    case h: HandleGeneral[e, r, a, b] => next[Plus[R, e], a, a, Z](h.p, new Last[Plus[R, e], a], new GeneralLevel[e, R, a, b, V, Z](h.h, fs, m))
    case h: HandleTail[e, r, a, b] => next[Plus[R, e], a, a, Z](h.p, new Last[Plus[R, e], a], new TailLevel[e, R, a, b, V, Z](h.h, fs, m))
    case h: HandleState[e, s, r, a, b] =>
      next[Plus[R, e], a, a, Z](h.p, new Last[Plus[R, e], a], new StateLevel[e, s, R, a, b, V, Z](h.h, h.s0, fs, m))
    case r: Remap[r1, r2, a] => next[r1, a, a, Z](r.p, new Last[r1, a], new RemapLevel[r1, R, a, V, Z](r.rename, fs, m))
    case r: Resume[r, x, b] => r.k.at(r.x, fs, m)
    case _ => throw new IllegalStateException(s"not a cold step: $c")
  }

  def pure[A, R](a: A): Prog[R, A] = Done(a)
  def flatMap[A, B, R](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Bind(p, f)
  def delay[A, R](p: => Prog[R, A]): Prog[R, A] = Delay(() => p)
  protected def performOp[E <: Effect, R, X](op: Op[E, X])(implicit in: In[E, R]): Prog[R, X] = Perf[R, E, X](placeOf(in), op)

  def handleLast[E <: Effect, R, A, B](p: Prog[Plus[R, E], A])(h: Handler[E, R, A, B]): Prog[R, B] = HandleGeneral(p, h)
  override def handleTailLast[E <: Effect, R, A, B](p: Prog[Plus[R, E], A])(h: TailHandler[E, R, A, B]): Prog[R, B] =
    HandleTail(p, h)
  override def handleStatefulLast[E <: Effect, S, R, A, B](p: Prog[Plus[R, E], A])(s0: S)(h: StatefulHandler[E, S, R, A, B]): Prog[R, B] =
    HandleState(p, s0, h)
  def run[A](p: Prog[PureRow, A]): A = loop[PureRow, A, A, A](p, new Last[PureRow, A], new Top[A])
}


