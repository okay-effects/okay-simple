package okay.simple.machine

import okay.api.{Cell, Effect, Effects, OpOf}
import scala.annotation.tailrec

/**
 * SCALA 3'S MACHINE, its own: typed by Scala 3's GADT inference throughout, no cast, no method that only one class
 * can mean. A program is `Prog[R, A]`, of the value `A` at the row `R`; an operation `Perf(at, op)`, its place a
 * typed path — `LastPlace` the last effect of `R + E`, `InitPlace` one further in, `OnePlace` a row of one — the
 * evidence `Split` itself, and the operation of the type its name says, `Op[E, X]`. The run is a `@tailrec` loop
 * over the program, the frames `Frames[R, X, V]` (the binds from `X` to the value `V` of the nearest level's body,
 * aligned) and the levels `Meta[R, V, Z]`, indexed by the row the program inside them is at: a handler of `E` at
 * `T + E`, its outside at `T`; a remapping of places (`split`, `embed`) from `R1` to `R2`.
 *
 * An operation's path is matched against the levels: a `LastPlace` at a handler's level is, by the types, that
 * handler's operation. Answered in place, the answer goes straight into the next frame. Taken by a general handler,
 * the levels crossed become `k`'s `Prefix`, typed, put back where `k` is resumed — a stateful level with the state it
 * had when `k` was taken. The loop holds the hot cases; a level entered or a `k` resumed is `cold`'s.
 */
/** the machine's data and its loop: in an object, static — no outer pointer in a node or a level, no outer test in
 * a match */
private[machine] object Machine:
  final abstract class PureRow
  final abstract class Plus[R, E]

  // -- PLACES: typed paths, the evidence itself

  /** the place of `E` in `R`, and `R` without it `Rest` */
  sealed abstract class Place[E <: Effect, R, Rest]:
    /** the places of `R` renamed to `Rest + E`'s: `E` moved to the end */
    def toEnd: Renaming[R, Plus[Rest, E]]
    /** `R` is `Rest + E` already: no renaming */
    def same: (R =:= Plus[Rest, E]) | Null

  final class OnePlace[E <: Effect] extends Place[E, E, PureRow]:
    def toEnd: Renaming[E, Plus[PureRow, E]] = new Renaming[E, Plus[PureRow, E]]:
      private val last = LastPlace[PureRow, E]()
      def apply[E2 <: Effect](at: Place[E2, E, ?]): Place[E2, Plus[PureRow, E], ?] = at match
        case _: OnePlace[e] => last
    def same: Null = null

  final class LastPlace[R, E <: Effect] extends Place[E, Plus[R, E], R]:
    def toEnd: Renaming[Plus[R, E], Plus[R, E]] = new Renaming[Plus[R, E], Plus[R, E]]:
      def apply[E2 <: Effect](at: Place[E2, Plus[R, E], ?]): Place[E2, Plus[R, E], ?] = at
    def same: Plus[R, E] =:= Plus[R, E] = summon[Plus[R, E] =:= Plus[R, E]]

  final class InitPlace[E <: Effect, R, G <: Effect, Rest](val s: Place[E, R, Rest]) extends Place[E, Plus[R, G], Plus[Rest, G]]:
    def toEnd: Renaming[Plus[R, G], Plus[Plus[Rest, G], E]] = new Renaming[Plus[R, G], Plus[Plus[Rest, G], E]]:
      // the places it renames to that are the same each time, made once
      private val gNow = InitPlace[G, Plus[Rest, G], E, Rest](LastPlace[Rest, G]())
      private val eNow = LastPlace[Plus[Rest, G], E]()
      private val inner = s.toEnd
      def apply[E2 <: Effect](at: Place[E2, Plus[R, G], ?]): Place[E2, Plus[Plus[Rest, G], E], ?] = at match
        case _: LastPlace[r, g] => gNow
        case ip: InitPlace[e2, r, g, rest] => inner(ip.s) match
          case _: LastPlace[r3, e3] => eNow
          case ip2: InitPlace[e4, r4, g4, rest4] => InitPlace(InitPlace[E2, Rest, G, rest4](ip2.s))
    def same: Null = null

  /** places renamed from `R1` to `R2`, typed */
  trait Renaming[R1, R2]:
    def apply[E <: Effect](at: Place[E, R1, ?]): Place[E, R2, ?]

  // -- THE HANDLERS, as the machine keeps them: of no implementation's instance, so static

  trait GH[E <: Effect, T, A, B]:
    def ret(a: A): Prog[T, B]
    def op[X](op: OpOf[E, X], k: X => Prog[T, B]): Prog[T, B]
  trait TH[E <: Effect, T, A, B]:
    def ret(a: A): Prog[T, B]
    def answer[X](op: OpOf[E, X]): X
  trait SH[E <: Effect, S, T, A, B]:
    def ret(s: S, a: A): Prog[T, B]
    def answer[X](cell: Cell[S], op: OpOf[E, X]): X

  // -- PROGRAMS

  sealed abstract class Prog[R, +A]
  final case class Done[R, A](a: A) extends Prog[R, A]
  final case class Bind[R, X, A](p: Prog[R, X], f: X => Prog[R, A]) extends Prog[R, A]
  final case class Delay[R, A](p: () => Prog[R, A]) extends Prog[R, A]
  final case class Perf[R, E <: Effect, X](at: Place[E, R, ?], op: OpOf[E, X]) extends Prog[R, X]
  final case class HandleGeneral[E <: Effect, R, A, B](p: Prog[Plus[R, E], A], h: GH[E, R, A, B]) extends Prog[R, B]
  final case class HandleTail[E <: Effect, R, A, B](p: Prog[Plus[R, E], A], h: TH[E, R, A, B]) extends Prog[R, B]
  final case class HandleState[E <: Effect, S, R, A, B](p: Prog[Plus[R, E], A], s0: S, h: SH[E, S, R, A, B])
      extends Prog[R, B]
  final case class Remap[R1, R2, A](p: Prog[R1, A], rename: Renaming[R1, R2]) extends Prog[R2, A]
  final case class Resume[R, X, B](k: K[R, X, B], x: X) extends Prog[R, B]

  // -- THE FRAMES: from a value `X` to the value `V` of the nearest level's body, at the row `R`

  sealed abstract class Frames[R, -X, +V]
  final class Last[R, V] extends Frames[R, V, V]
  final class Then[R, X, Y, V](val f: X => Prog[R, Y], val rest: Frames[R, Y, V]) extends Frames[R, X, V]

  // -- THE LEVELS: the program inside at `R`, its body's value `V`, the run's `Z`

  sealed abstract class Meta[R, V, Z]
  final class Top[Z] extends Meta[PureRow, Z, Z]

  /** a handler of `E`'s level: the program inside at `T + E`, its body's value `A`; the frames outside it from `B`
   * to `W` at `T`, the levels outside them */
  sealed abstract class HandlerLevel[E <: Effect, T, A, B, W, Z](val outside: Frames[T, B, W], val m: Meta[T, W, Z])
      extends Meta[Plus[T, E], A, Z]:
    def ret(a: A): Prog[T, B]
    /** `pre` with this level after it, as it is now: what a `k` taken past it puts back */
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W]

  final class GeneralLevel[E <: Effect, T, A, B, W, Z](val h: GH[E, T, A, B], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends HandlerLevel[E, T, A, B, W, Z](outside, m):
    def ret(a: A): Prog[T, B] = h.ret(a)
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W]:
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(GeneralLevel(h, outside, m2))

  /** answering in place */
  sealed abstract class InPlace[E <: Effect, T, A, B, W, Z](outside: Frames[T, B, W], m: Meta[T, W, Z])
      extends HandlerLevel[E, T, A, B, W, Z](outside, m):
    def answer[X](op: OpOf[E, X]): X

  final class TailLevel[E <: Effect, T, A, B, W, Z](val h: TH[E, T, A, B], outside: Frames[T, B, W],
      m: Meta[T, W, Z]) extends InPlace[E, T, A, B, W, Z](outside, m):
    def ret(a: A): Prog[T, B] = h.ret(a)
    def answer[X](op: OpOf[E, X]): X = h.answer[X](op)
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] = new Prefix[R0, V0, T, W]:
      def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(TailLevel(h, outside, m2))

  /** a stateful handler's level: its state the cell its clauses write */
  final class StateLevel[E <: Effect, S, T, A, B, W, Z](val h: SH[E, S, T, A, B], var state: S,
      outside: Frames[T, B, W], m: Meta[T, W, Z]) extends InPlace[E, T, A, B, W, Z](outside, m) with Cell[S]:
    def get: S = state
    def set(s: S): Unit = state = s
    def ret(a: A): Prog[T, B] = h.ret(state, a)
    def answer[X](op: OpOf[E, X]): X = h.answer[X](this, op)
    /** put back with the state it has now, each time */
    def crossed[R0, V0](pre: Prefix[R0, V0, Plus[T, E], A]): Prefix[R0, V0, T, W] =
      val now = state
      new Prefix[R0, V0, T, W]:
        def put[Z2](m2: Meta[T, W, Z2]): Meta[R0, V0, Z2] = pre.put(StateLevel(h, now, outside, m2))

  /** a remapping's level: the program inside at `R1`, its places renamed to `R2`'s outside it */
  final class RemapLevel[R1, R2, A, W, Z](val rename: Renaming[R1, R2], val outside: Frames[R2, A, W],
      val m: Meta[R2, W, Z]) extends Meta[R1, A, Z]:
    def crossed[R0, V0](pre: Prefix[R0, V0, R1, A]): Prefix[R0, V0, R2, W] = new Prefix[R0, V0, R2, W]:
      def put[Z2](m2: Meta[R2, W, Z2]): Meta[R0, V0, Z2] = pre.put(RemapLevel(rename, outside, m2))

  /** levels taken off, the innermost's body at `RIn` of `VIn`, the outermost's at `ROut` of `VOut`: put back on any */
  trait Prefix[RIn, VIn, ROut, VOut]:
    def put[Z](m: Meta[ROut, VOut, Z]): Meta[RIn, VIn, Z]
  private def none[R, V]: Prefix[R, V, R, V] = new Prefix[R, V, R, V]:
    def put[Z](m: Meta[R, V, Z]): Meta[R, V, Z] = m

  /** `k`: the frames inside, the levels crossed, the general handler's level — resumed at its outside `T` */
  final class K[T, X, B](val resume: [W, Z] => (X, Frames[T, B, W], Meta[T, W, Z]) => Next[Z])

  /** where the run goes on, after a cold step */
  final class Next[Z](val go: Go[?, ?, ?, Z])
  final case class Go[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z])

  // -- THE LOOP

  /** the handler answering `at` in place, walked by the path; `null`: a general one, or a handler not in place */
  @tailrec private def inPlace[E <: Effect, R, V, Z](at: Place[E, R, ?], m: Meta[R, V, Z]): InPlace[E, ?, ?, ?, ?, ?] | Null =
    m match
      case l: HandlerLevel[e, t, a, b, w, z] => at match
        case _: LastPlace[r, e2] => l match
          case ip: InPlace[e3, t3, a3, b3, w3, z3] => ip
          case _ => null
        case ip: InitPlace[e2, r, g, rest] => inPlace[E, t, w, Z](ip.s, l.m)
      case r: RemapLevel[r1, r2, a, w, z] => inPlace[E, r2, w, Z](r.rename(at), r.m)
      case _: Top[z] => throw IllegalStateException(s"an operation no handler took, at $at")

  @tailrec private def loop[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Z = c match
    case d: Done[r, x] => fs match
      case t: Then[r2, a, y, v] => loop[R, y, V, Z](t.f(d.a), t.rest, m)
      case _: Last[r2, v] => m match
        case _: Top[z] => d.a
        case _ =>
          val g = cold[R, X, V, Z](c, fs, m).go
          loop(g.c, g.fs, g.m)
    case b: Bind[r, y, x] => loop[R, y, V, Z](b.p, Then[R, y, x, V](b.f, fs), m)
    case p: Perf[r, e, x] => inPlace[e, R, V, Z](p.at, m) match
      case h: InPlace[e2, t, a, b, w, z] =>
        // answered in place, the answer straight into the next frame: no `Done` made
        val x = h.answer[x](p.op)
        fs match
          case t: Then[r2, a2, y, v] => loop[R, y, V, Z](t.f(x), t.rest, m)
          case _ => loop[R, X, V, Z](Done(x), fs, m)
      case null =>
        val g = walk[e, R, x, V, R, V, Z](p.at, p.op, fs, none[R, V], m).go
        loop(g.c, g.fs, g.m)
    case d: Delay[r, x] => loop[R, X, V, Z](d.p(), fs, m)
    case _ =>
      val g = cold[R, X, V, Z](c, fs, m).go
      loop(g.c, g.fs, g.m)

  /** the loop's cold steps: a level left, a level entered, a `k` resumed */
  private def cold[R, X, V, Z](c: Prog[R, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Next[Z] = c match
    case d: Done[r, x] => fs match
      case t: Then[r2, a, y, v] => Next(Go[R, y, V, Z](t.f(d.a), t.rest, m))
      case _: Last[r2, v] => m match
        case l: HandlerLevel[e, t, a, b, w, z] => Next(Go[t, b, w, Z](l.ret(d.a), l.outside, l.m))
        case l: RemapLevel[r1, r2, a, w, z] => Next(Go[r2, a, w, Z](Done(d.a), l.outside, l.m))
        case _: Top[z] => throw IllegalStateException("the top is the loop's")
    case h: HandleGeneral[e, r, a, b] => Next(Go[Plus[R, e], a, a, Z](h.p, Last(), GeneralLevel[e, R, a, b, V, Z](h.h, fs, m)))
    case h: HandleTail[e, r, a, b] => Next(Go[Plus[R, e], a, a, Z](h.p, Last(), TailLevel[e, R, a, b, V, Z](h.h, fs, m)))
    case h: HandleState[e, s, r, a, b] =>
      Next(Go[Plus[R, e], a, a, Z](h.p, Last(), StateLevel[e, s, R, a, b, V, Z](h.h, h.s0, fs, m)))
    case r: Remap[r1, r2, a] => Next(Go[r1, a, a, Z](r.p, Last(), RemapLevel[r1, R, a, V, Z](r.rename, fs, m)))
    case r: Resume[r, x, b] => r.k.resume[V, Z](r.x, fs, m)
    case p: Perf[r, e, x] => walk[e, R, x, V, R, V, Z](p.at, p.op, fs, none[R, V], m)
    case b: Bind[r, y, x] => Next(Go[R, y, V, Z](b.p, Then[R, y, x, V](b.f, fs), m))
    case d: Delay[r, x] => Next(Go[R, X, V, Z](d.p(), fs, m))

  /** an operation to its general handler: the levels it crosses taken onto `pre`, `k` made, the clause run outside */
  @tailrec private def walk[E <: Effect, R0, X, V0, R, V, Z](at: Place[E, R, ?], op: OpOf[E, X], inner: Frames[R0, X, V0],
      pre: Prefix[R0, V0, R, V], m: Meta[R, V, Z]): Next[Z] = m match
    case l: HandlerLevel[e, t, a, b, w, z] => at match
      case _: LastPlace[r, e2] => l match
        case g: GeneralLevel[e3, t3, a3, b3, w3, z3] =>
          val k = K[t, X, b]([W2, Z2] => (x: X, fs: Frames[t, b, W2], m2: Meta[t, W2, Z2]) =>
            Next(Go[R0, X, V0, Z2](Done(x), inner, pre.put(GeneralLevel[E, t, a, b, W2, Z2](g.h, fs, m2)))))
          Next(Go[t, b, w, Z](g.h.op[X](op, (x: X) => Resume(k, x)), g.outside, g.m))
        case _ => throw IllegalStateException("answered in place")
      case ip: InitPlace[e2, r, g, rest] => walk[E, R0, X, V0, t, w, Z](ip.s, op, inner, l.crossed(pre), l.m)
    case r: RemapLevel[r1, r2, a, w, z] => walk[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, r.crossed(pre), r.m)
    case _: Top[z] => throw IllegalStateException(s"an operation no handler took, at $at")

  def run[A](p: Prog[PureRow, A]): A = loop[PureRow, A, A, A](p, Last(), Top())

/** the interface over the machine: its types the object's, a handler kept as the object's shape of it */
private[machine] class Machine extends Effects:
  import Machine.*

  type Pure = PureRow
  type +[R, E <: Effect] = Plus[R, E]
  type ![+A, R] = Prog[R, A]

  type Split[E <: Effect, R, Rest] = Place[E, R, Rest]
  type In[E <: Effect, R] = Place[E, R, ?]
  type Sub[R, S] = Renaming[R, S]

  implicit def splitOne[E <: Effect]: Split[E, E, Pure] = OnePlace[E]()
  implicit def splitLast[R, E <: Effect]: Split[E, R + E, R] = LastPlace[R, E]()
  implicit def splitInit[E <: Effect, R, G <: Effect, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    InitPlace[E, R, G, Rest](s)
  implicit def in[E <: Effect, R, Rest](implicit s: Split[E, R, Rest]): In[E, R] = s
  implicit def subPure[S]: Sub[Pure, S] = new Renaming[PureRow, S]:
    def apply[E <: Effect](at: Place[E, PureRow, ?]): Place[E, S, ?] = throw IllegalStateException("no place in Pure")
  implicit def subOne[E <: Effect, S](implicit e: In[E, S]): Sub[E, S] = new Renaming[E, S]:
    def apply[E2 <: Effect](at: Place[E2, E, ?]): Place[E2, S, ?] = at match
      case _: OnePlace[e] => e
  implicit def subMore[R, E <: Effect, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = new Renaming[Plus[R, E], S]:
    def apply[E2 <: Effect](at: Place[E2, Plus[R, E], ?]): Place[E2, S, ?] = at match
      case _: LastPlace[r1, e1] => e
      case ip: InitPlace[e2, r2, g, rest] => r(ip.s)

  // -- THE INTERFACE

  def pure[A, R](a: A): Prog[R, A] = Done(a)
  def flatMap[A, B, R](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Bind(p, f)
  def delay[A, R](p: => Prog[R, A]): Prog[R, A] = Delay(() => p)
  protected def performOp[E <: Effect, R, X](op: Op[E, X])(implicit in: In[E, R]): Prog[R, X] = Perf[R, E, X](in, op)

  def split[E <: Effect, R, Rest, A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Plus[Rest, E], A] = s.same match
    case ev: (R =:= Plus[Rest, E]) => ev.substituteCo[[r] =>> Prog[r, A]](p)
    case null => Remap(p, s.toEnd)
  def embed[R, S, A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Remap(p, s)

  def handleLast[E <: Effect, R, A, B](p: Prog[Plus[R, E], A])(h: Handler[E, R, A, B]): Prog[R, B] =
    HandleGeneral(p, new GH[E, R, A, B]:
      def ret(a: A): Prog[R, B] = h.ret(a)
      def op[X](op: OpOf[E, X], k: X => Prog[R, B]): Prog[R, B] = h.op[X](op, k))
  override def handleTailLast[E <: Effect, R, A, B](p: Prog[Plus[R, E], A])(h: TailHandler[E, R, A, B]): Prog[R, B] =
    HandleTail(p, new TH[E, R, A, B]:
      def ret(a: A): Prog[R, B] = h.ret(a)
      def answer[X](op: OpOf[E, X]): X = h.answer[X](op))
  override def handleStatefulLast[E <: Effect, S, R, A, B](p: Prog[Plus[R, E], A])(s0: S)(h: StatefulHandler[E, S, R, A, B]): Prog[R, B] =
    HandleState(p, s0, new SH[E, S, R, A, B]:
      def ret(s: S, a: A): Prog[R, B] = h.ret(s, a)
      def answer[X](cell: Cell[S], op: OpOf[E, X]): X = h.answer[X](cell, op))
  def run[A](p: Prog[PureRow, A]): A = Machine.run(p)
