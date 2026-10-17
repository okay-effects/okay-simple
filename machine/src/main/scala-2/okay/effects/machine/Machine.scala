package okay.effects.machine

import okay.effects.{AbortClauses, Cell, Clauses, Effects, StatefulClauses, StatefulTailClauses, Sum, TailClauses, Void}
import scala.annotation.tailrec

/**
 * SCALA 2'S MACHINE. A program is `Prog[R, A]`, of the value `A` at the row `R`; an operation `Perf(at, op)`, its
 * place a number — the handlers out to its own, `Remap`s renaming it — and the operation. The run is a `@tailrec`
 * loop over the program, the FRAMES `Frames[R, X, V]` (the binds from `X` to the value `V` of the nearest level's
 * body, aligned) and the LEVELS `Meta[R, V, Z]` (a handler of `E` at `T + E`, its outside at `T`; a remapping of
 * places from `R1` to `R2`).
 *
 * Scala 2 has no GADT inference, so a match does not refine a level's types: each level does for itself what needs
 * its types — `returned` (a value given back through it), `stepOut` (crossed by an operation), `found` (an operation
 * of its own) — and the walk to an operation's handler reads the levels' fields, `kind` and `outer`, with no call.
 * Its TRUSTED STEPS: an operation found at its handler is of its handler's effect (`opAs`); answered in place, its
 * answer of its type; `split` at the last place no move (`sameRow`). The interface's evidence, found by the row's
 * types, makes each so.
 *
 * An operation answered in place goes straight into the next frame, no `Done` made; taken by a general handler, the
 * levels crossed become `k`'s `Prefix`, put back where `k` is resumed — a stateful level with the state of its time.
 * The machine's data in an object: no outer pointer in a node or a level.
 */
private[effects] object Machine {
  type PureRow[X] = Void[X]
  /** the row `R + E`: `Plus[R, E]#L` */
  final abstract class Plus[R[_], E[_]] { type L[X] = Sum[R, E, X] }

  /** a place: the number of handlers out to the operation's own, one object per depth */
  final class Place(val n: Int)
  private val places: Array[Place] = Array.tabulate(64)(new Place(_))
  def placeAt(i: Int): Place = if (i < places.length) places(i) else new Place(i)

  /** THE TRUSTED STEP: an operation found at its handler, of its handler's effect */
  def opAs[E[_], X](op: Any): E[X] = op.asInstanceOf[E[X]]

  // -- PROGRAMS

  sealed abstract class Prog[R[_], +A]
  final case class Done[R[_], A](a: A) extends Prog[R, A]
  final case class Bind[R[_], X, A](p: Prog[R, X], f: X => Prog[R, A]) extends Prog[R, A]
  /** a value through `f`: one step, no program made of it */
  final case class Mapping[R[_], X, A](p: Prog[R, X], f: X => A) extends Prog[R, A]
  final case class Delay[R[_], A](p: () => Prog[R, A]) extends Prog[R, A]
  final case class Perf[R[_], X](at: Int, op: Any) extends Prog[R, X]
  /** a handler installed over `p`: `install` makes its level, of the handler's shape, its state fresh */
  final case class Handle[E[_], T[_], A, B](p: Prog[Plus[T, E]#L, A], install: Install[E, T, A, B]) extends Prog[T, B]
  final case class Remap[R1[_], R2[_], A](p: Prog[R1, A], rename: Int => Int) extends Prog[R2, A]
  final case class Resume[T[_], X, B](k: K[T, X, B], x: X) extends Prog[T, B]

  trait Install[E[_], T[_], A, B] {
    def apply[W, Z](outside: Frames[T, B, W], m: Meta[T, W, Z]): Meta[Plus[T, E]#L, A, Z]
  }

  // -- THE FRAMES: from a value `X` to the value `V` of the nearest level's body, at the row `R`

  sealed abstract class Frames[R[_], -X, +V]
  /** the end: its value the level's body's — typed by the bounds a match on it gives, `X <: V` */
  final class Last[R[_], V] extends Frames[R, V, V] { def last(x: V): V = x }
  final class Then[R[_], X, Y, V](val f: X => Prog[R, Y], val rest: Frames[R, Y, V]) extends Frames[R, X, V]
  /** a map pending: the value through `f`, on into `rest` */
  final class Mapped[R[_], X, Y, V](val f: X => Y, val rest: Frames[R, Y, V]) extends Frames[R, X, V]

  // -- THE LEVELS

  final val TopKind = 0
  final val HandlerKind = 1
  final val RemapKind = 2

  /** a level by its fields, read by the walk with no call: its kind, the one further out, a remapping's renaming */
  sealed abstract class Level0(final val kind: Int, final val outer: Level0, final val rename: Int => Int)

  /** a level answering in place: of `Tail` and `State` handlers */
  sealed trait Answering { def answerAny(op: Any): Any }

  /** the levels the program at the row `R` runs in, from the value `V` of the nearest one's body to the run's `Z` */
  sealed abstract class Meta[R[_], V, Z](kind: Int, outer: Level0, rename: Int => Int)
      extends Level0(kind, outer, rename) {
    /** the body's value given back through this level: where the run goes on, or its end */
    def returned(v: V): Next[Z]
    /** this level crossed by an operation: `pre` with it after, and the levels outside it */
    def stepOut[R0[_], V0](pre: Prefix[R0, V0, R, V]): Out[R0, V0, Z]
    /** an operation found at this level: of its handler, `inner` the frames inside, `pre` the levels crossed */
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, V]): Go[Z]
  }

  /** the top: the run's value its end. An operation finds it only if it is of `Void`, of no value: absurd */
  final class Top[Z] extends Meta[PureRow, Z, Z](TopKind, null, null) {
    def returned(v: Z): Next[Z] = End(v)
    def stepOut[R0[_], V0](pre: Prefix[R0, V0, PureRow, Z]): Out[R0, V0, Z] = out(pre, this)
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, PureRow, Z]): Go[Z] =
      opAs[Void, Any](op).absurd
  }

  /** a handler of `E`'s level: the program inside at `T + E`, its body's value `A`; the frames outside it from `B` to
   * `W` at `T`, the levels outside them */
  sealed abstract class HandlerLevel[E[_], T[_], A, B, W, Z](final val outside: Frames[T, B, W], final val m: Meta[T, W, Z])
      extends Meta[Plus[T, E]#L, A, Z](HandlerKind, m, null) {
    /** `pre` with this level after it, as it is now: what a `k` taken past it puts back */
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W]
    final def stepOut[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Out[R0, V0, Z] = out(crossed(pre), m)
  }

  final class GeneralLevel[E[_], T[_], A, B, W, Z](h: Clauses[E, A, Prog[T, B]], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m) {
    def returned(v: A): Next[Z] = go[T, B, W, Z](h.ret(v), outside, m)
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new GeneralLevel(h, outside, m2))
    }
    /** `k` the frames inside and the levels crossed, put back on this level where it is resumed */
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E]#L, A]): Go[Z] = {
      val k = new K[T, X, B] {
        def at[W2, Z2](x: X, fs: Frames[T, B, W2], m2: Meta[T, W2, Z2]): Go[Z2] =
          go[R0, X, V0, Z2](Done(x), inner, pre.put(new GeneralLevel(h, fs, m2)))
      }
      go[T, B, W, Z](h.op[X](opAs[E, X](op), (x: X) => Resume(k, x)), outside, m)
    }
  }

  /** a general handler's level with a state of its own: the state of its time; `k` put back with the state given */
  final class StateKLevel[E[_], S, T[_], A, B, W, Z](h: StatefulClauses[E, S, A, Prog[T, B]], state: S,
      outside: Frames[T, B, W], m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m) {
    def returned(v: A): Next[Z] = go[T, B, W, Z](h.ret(state, v), outside, m)
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new StateKLevel(h, state, outside, m2))
    }
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E]#L, A]): Go[Z] = {
      def k(s1: S) = new K[T, X, B] {
        def at[W2, Z2](x: X, fs: Frames[T, B, W2], m2: Meta[T, W2, Z2]): Go[Z2] =
          go[R0, X, V0, Z2](Done(x), inner, pre.put(new StateKLevel(h, s1, fs, m2)))
      }
      go[T, B, W, Z](h.op[X](state, opAs[E, X](op), (x: X, s1: S) => Resume(k(s1), x)), outside, m)
    }
  }

  /** a handler never resuming `k`: an operation found here leaves at once with the clause's result */
  final class AbortLevel[E[_], T[_], A, B, W, Z](h: AbortClauses[E, A, Prog[T, B]], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m) {
    def returned(v: A): Next[Z] = go[T, B, W, Z](h.ret(v), outside, m)
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new AbortLevel(h, outside, m2))
    }
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E]#L, A]): Go[Z] =
      go[T, B, W, Z](h.abort[X](opAs[E, X](op)), outside, m)
  }

  /** answering in place: found by the walk, answered there, the levels crossed put back */
  final class TailLevel[E[_], T[_], A, B, W, Z](h: TailClauses[E, A, Prog[T, B]], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m) with Answering {
    def returned(v: A): Next[Z] = go[T, B, W, Z](h.ret(v), outside, m)
    def answerAny(op: Any): Any = h.answer[Any](opAs[E, Any](op))
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W] {
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new TailLevel(h, outside, m2))
    }
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E]#L, A]): Go[Z] =
      go[R0, X, V0, Z](Done(h.answer[X](opAs[E, X](op))), inner, pre.put(this))
  }

  /** a stateful handler's level answering in place: its state the cell its clauses write */
  final class StateLevel[E[_], S, T[_], A, B, W, Z](h: StatefulTailClauses[E, S, A, Prog[T, B]], var state: S,
      outside: Frames[T, B, W], m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m)
      with Answering with Cell[S] {
    def get: S = state
    def set(s: S): Unit = state = s
    def returned(v: A): Next[Z] = go[T, B, W, Z](h.ret(state, v), outside, m)
    def answerAny(op: Any): Any = h.answer[Any](this, opAs[E, Any](op))
    /** put back with the state it has now, each time */
    def crossed[R0[_], V0](pre: Prefix[R0, V0, Plus[T, E]#L, A]): Prefix[R0, V0, T, W] = {
      val now = state
      new Prefix[R0, V0, T, W] { def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(new StateLevel(h, now, outside, m2)) }
    }
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, Plus[T, E]#L, A]): Go[Z] =
      go[R0, X, V0, Z](Done(h.answer[X](this, opAs[E, X](op))), inner, pre.put(this))
  }

  /** a remapping's level: the program inside at `R1`, its places renamed by `rename` to `R2`'s outside it */
  final class RemapLevel[R1[_], R2[_], A, W, Z](rename: Int => Int, final val outside: Frames[R2, A, W],
      final val m: Meta[R2, W, Z]) extends Meta[R1, A, Z](RemapKind, m, rename) {
    def returned(v: A): Next[Z] = go[R2, A, W, Z](Done(v), outside, m)
    def crossed[R0[_], V0](pre: Prefix[R0, V0, R1, A]): Prefix[R0, V0, R2, W] = new Prefix[R0, V0, R2, W] {
      def put[Z2](m2: Meta[R2, W, Z2]): Meta[R0, V0, Z2] = pre.put(new RemapLevel(rename, outside, m2))
    }
    def stepOut[R0[_], V0](pre: Prefix[R0, V0, R1, A]): Out[R0, V0, Z] = out(crossed(pre), m)
    /** an operation at place 0 here: renamed, on outside */
    def found[R0[_], X, V0](op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R1, A]): Go[Z] =
      walk[R0, X, V0, R2, W, Z](rename(0), op, inner, crossed(pre), m)
  }

  /** levels taken off, the innermost's body at `RIn` of `VIn`, the outermost's at `ROut` of `VOut`: put back on any */
  trait Prefix[RIn[_], VIn, ROut[_], VOut] { def put[Z](m: Meta[ROut, VOut, Z]): Meta[RIn, VIn, Z] }
  def none[R[_], V]: Prefix[R, V, R, V] = new Prefix[R, V, R, V] { def put[Z](m: Meta[R, V, Z]): Meta[R, V, Z] = m }

  /** a level crossed: the prefix with it, the levels outside — their types its own */
  abstract class Out[R0[_], V0, Z] {
    type R[_]
    type V
    val pre: Prefix[R0, V0, R, V]
    val m: Meta[R, V, Z]
  }
  def out[R0[_], V0, R1[_], V1, Z](p: Prefix[R0, V0, R1, V1], m1: Meta[R1, V1, Z]): Out[R0, V0, Z] = new Out[R0, V0, Z] {
    type R[x] = R1[x]
    type V = V1
    val pre = p
    val m = m1
  }

  /** `k`: resumed at the row `T` with an `X`, where the frames from `B` and the levels are */
  trait K[T[_], X, B] { def at[W, Z](x: X, fs: Frames[T, B, W], m: Meta[T, W, Z]): Go[Z] }

  /** where the run goes on — a program, its frames, the levels, their types its own — or its end */
  sealed abstract class Next[Z]
  abstract class Go[Z] extends Next[Z] {
    type R[_]
    type X
    type V
    val c: Prog[R, X]
    val fs: Frames[R, X, V]
    val m: Meta[R, V, Z]
  }
  final case class End[Z](z: Z) extends Next[Z]
  def go[R0[_], X0, V0, Z](c0: Prog[R0, X0], fs0: Frames[R0, X0, V0], m0: Meta[R0, V0, Z]): Go[Z] = new Go[Z] {
    type R[x] = R0[x]
    type X = X0
    type V = V0
    val c = c0
    val fs = fs0
    val m = m0
  }

  // -- THE LOOP

  @tailrec def loop[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Z = c match {
    case d: Done[r, x] => fs match {
      case t: Then[r2, a, y, v] => loop[R, y, V, Z](t.f(d.a), t.rest, m)
      case t: Mapped[r2, a, y, v] => loop[R, y, V, Z](Done(t.f(d.a)), t.rest, m)
      case l: Last[r2, v] => m.returned(l.last(d.a)) match {
        case g: Go[Z @unchecked] => loop[g.R, g.X, g.V, Z](g.c, g.fs, g.m)
        case e: End[Z @unchecked] => e.z
      }
    }
    case b: Bind[r, y, x] => loop[R, y, V, Z](b.p, new Then[R, y, x, V](b.f, fs), m)
    case b: Mapping[r, y, x] => loop[R, y, V, Z](b.p, new Mapped[R, y, x, V](b.f, fs), m)
    case p: Perf[r, x] =>
      // the handler, walked to by the levels' fields: a remapping renames the place, a handler counts one
      var n = p.at
      var l: Level0 = m
      while (l.kind == RemapKind || (n != 0 && l.kind == HandlerKind)) {
        if (l.kind == RemapKind) n = l.rename(n) else n -= 1
        l = l.outer
      }
      l match {
        case a: Answering =>
          // answered in place, the answer straight into the next frame: no `Done` made
          val x = a.answerAny(p.op).asInstanceOf[x]
          fs match {
            case t: Then[r2, a2, y, v] => loop[R, y, V, Z](t.f(x), t.rest, m)
            case _ => loop[R, X, V, Z](Done(x), fs, m)
          }
        case _ =>
          val g = walk[R, x, V, R, V, Z](p.at, p.op, fs, none[R, V], m)
          loop[g.R, g.X, g.V, Z](g.c, g.fs, g.m)
      }
    case d: Delay[r, x] => loop[R, X, V, Z](d.p(), fs, m)
    case _ =>
      val g = cold[R, X, V, Z](c, fs, m)
      loop[g.R, g.X, g.V, Z](g.c, g.fs, g.m)
  }

  /** the loop's cold steps: a level entered, a `k` resumed; a step of the loop's own given back to it */
  private def cold[R[_], X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Go[Z] = c match {
    case h: Handle[e, r, a, b] => go[Plus[R, e]#L, a, a, Z](h.p, new Last[Plus[R, e]#L, a], h.install[V, Z](fs, m))
    case r: Remap[r1, r2, a] => go[r1, a, a, Z](r.p, new Last[r1, a], new RemapLevel[r1, R, a, V, Z](r.rename, fs, m))
    case r: Resume[r, x, b] => r.k.at[V, Z](r.x, fs, m)
    case _ => go[R, X, V, Z](c, fs, m)
  }

  /** an operation to its handler: the levels it crosses taken onto `pre`, each step the level's own, typed */
  @tailrec def walk[R0[_], X, V0, R[_], V, Z](n: Int, op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, V],
      m: Meta[R, V, Z]): Go[Z] =
    if (m.kind == RemapKind) {
      val o = m.stepOut(pre)
      walk[R0, X, V0, o.R, o.V, Z](m.rename(n), op, inner, o.pre, o.m)
    } else if (n == 0 || m.kind == TopKind) m.found[R0, X, V0](op, inner, pre)
    else {
      val o = m.stepOut(pre)
      walk[R0, X, V0, o.R, o.V, Z](n - 1, op, inner, o.pre, o.m)
    }

  def run[A](p: Prog[PureRow, A]): A = loop[PureRow, A, A, A](p, new Last[PureRow, A], new Top[A])
}

/** the interface over the machine: its types the object's, a handler installed as its level */
private[effects] class Machine extends Effects {
  import Machine._

  type ![+A, R[_]] = Prog[R, A]
  type Split[E[_], R[_], Rest[_]] = Place
  type In[E[_], R[_]] = Place
  type Sub[R[_], S[_]] = Vector[Int]

  implicit def splitOne[E[_]]: Place = placeAt(0)
  implicit def splitLast[R[_], E[_]]: Place = placeAt(0)
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Place): Place = placeAt(s.n + 1)
  implicit def subPure[S[_]]: Vector[Int] = Vector()
  implicit def subOne[E[_], S[_]](implicit e: Place): Vector[Int] = Vector(e.n)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Vector[Int], e: Place): Vector[Int] = e.n +: r

  def pure[A, R[_]](a: A): Prog[R, A] = Done(a)
  def flatMap[A, B, R[_]](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Bind(p, f)
  override def map[A, B, R[_]](p: Prog[R, A])(f: A => B): Prog[R, B] = Mapping(p, f)
  def delay[A, R[_]](p: => Prog[R, A]): Prog[R, A] = Delay(() => p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: Place): Prog[R, X] = Perf[R, X](in.n, op)

  /** THE TRUSTED STEP of `split`: at the last place, the row as it is, `Rest + E` */
  private def sameRow[R[_], R2[_], A](p: Prog[R, A]): Prog[R2, A] = p.asInstanceOf[Prog[R2, A]]

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Place): Prog[Plus[Rest, E]#L, A] = {
    val at = s.n
    if (at == 0) sameRow[R, Plus[Rest, E]#L, A](p)
    else Remap[R, Plus[Rest, E]#L, A](p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Vector[Int]): Prog[S, A] = Remap[R, S, A](p, s)

  def handleLast[E[_], R[_], A, B](p: Prog[Plus[R, E]#L, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Handle(p, new Install[E, R, A, B] {
      def apply[W, Z](o: Frames[R, B, W], m: Meta[R, W, Z]) = new GeneralLevel(h, o, m)
    })
  override def handleLast[E[_], R[_], A, B](p: Prog[Plus[R, E]#L, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Handle(p, new Install[E, R, A, B] {
      def apply[W, Z](o: Frames[R, B, W], m: Meta[R, W, Z]) = new TailLevel(h, o, m)
    })
  override def handleLast[E[_], R[_], A, B](p: Prog[Plus[R, E]#L, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Handle(p, new Install[E, R, A, B] {
      def apply[W, Z](o: Frames[R, B, W], m: Meta[R, W, Z]) = new AbortLevel(h, o, m)
    })
  override def handleLast[E[_], S, R[_], A, B](p: Prog[Plus[R, E]#L, A], s0: S, h: StatefulTailHandler[E, S, R, A, B]): Prog[R, B] =
    Handle(p, new Install[E, R, A, B] {
      def apply[W, Z](o: Frames[R, B, W], m: Meta[R, W, Z]) = new StateLevel(h, s0, o, m)
    })
  override def handleLast[E[_], S, R[_], A, B](p: Prog[Plus[R, E]#L, A], s0: S, h: StatefulHandler[E, S, R, A, B]): Prog[R, B] =
    Handle(p, new Install[E, R, A, B] {
      def apply[W, Z](o: Frames[R, B, W], m: Meta[R, W, Z]) = new StateKLevel(h, s0, o, m)
    })
  def run[A](p: Prog[PureRow, A]): A = Machine.run(p)
}
