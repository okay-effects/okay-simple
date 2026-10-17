package okay.effects.tagless

import okay.effects.{AbortClauses, Clauses, Effects, Runner, StatefulClauses, StatefulTailClauses, Std, Sum, TailClauses, Void}

/** A PROGRAM OF NO IMPLEMENTATION: made, when it is run, on the implementation given — `on(fx)`, `fx`'s own program.
 * A value: made once, run on any implementation */
abstract class Program[+A, R[_]]:
  def on(fx: Effects): fx.![A, R]
  /** made on `fx` as the `depth`-th of a chain: a bind's or a map's own; any other, `on` */
  private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] = on(fx)

object Program:
  /** a chain of binds and maps made by nested calls, so many deep at most; one deeper `delay`ed, made by `fx`'s run —
   * a loop — when it gets there: a chain of any length in constant stack, one `delay` in so many */
  private inline val Deepest = 64

  final class Pure[A, R[_]](a: A) extends Program[A, R]:
    def on(fx: Effects): fx.![A, R] = fx.pure[A, R](a)
  final class Bind[X, A, R[_]](p: Program[X, R], f: X => Program[A, R]) extends Program[A, R]:
    def on(fx: Effects): fx.![A, R] = onAt(fx, 0)
    override private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] =
      fx.flatMap[X, A, R](before[X, R](fx, p, depth))((x: X) => f(x).on(fx))
  final class Mapped[X, A, R[_]](p: Program[X, R], f: X => A) extends Program[A, R]:
    def on(fx: Effects): fx.![A, R] = onAt(fx, 0)
    override private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] = fx.map[X, A, R](before[X, R](fx, p, depth))(f)
  /** an operation of `E`, at its place in `R` */
  final class Performed[E[_], R[_], X](op: E[X], in: InRow[E, R]) extends Program[X, R]:
    def on(fx: Effects): fx.![X, R] = fx.perform[E].apply[R, X](op)(using in.on(fx))
  /** any other word's: made by `make`, a function of the implementation */
  final class Of[A, R[_]](make: (fx: Effects) => fx.![A, R]) extends Program[A, R]:
    def on(fx: Effects): fx.![A, R] = make(fx)

  /** the program before a bind or a map, made on `fx`: one deeper, or at `Deepest` `delay`ed and from 0 again */
  private inline def before[X, R[_]](fx: Effects, p: Program[X, R], depth: Int): fx.![X, R] =
    if depth < Deepest then p.onAt(fx, depth + 1) else fx.delay[X, R](p.onAt(fx, 0))

/** `E` is in `R`, on any implementation: `fx`'s own evidence, made on it */
trait InRow[E[_], R[_]]:
  def on(fx: Effects): fx.In[E, R]
/** `E` in `R` and `R` without it `Rest`, on any implementation */
trait SplitRow[E[_], R[_], Rest[_]] extends InRow[E, R]:
  def on(fx: Effects): fx.Split[E, R, Rest]

/** each implementation's derivations, made on it */
object InRow:
  final class One[E[_]] extends SplitRow[E, E, Void]:
    def on(fx: Effects): fx.Split[E, E, Void] = fx.splitOne[E]
  final class Last[R[_], E[_]] extends SplitRow[E, [x] =>> Sum[R, E, x], R]:
    def on(fx: Effects): fx.Split[E, [x] =>> Sum[R, E, x], R] = fx.splitLast[R, E]
  final class Init[E[_], R[_], G[_], Rest[_]](s: SplitRow[E, R, Rest])
      extends SplitRow[E, [x] =>> Sum[R, G, x], [x] =>> Sum[Rest, G, x]]:
    def on(fx: Effects): fx.Split[E, [x] =>> Sum[R, G, x], [x] =>> Sum[Rest, G, x]] = fx.splitInit[E, R, G, Rest](using s.on(fx))


/** every effect of `R` is in `S`, on any implementation */
trait SubRow[R[_], S[_]]:
  def on(fx: Effects): fx.Sub[R, S]

object SubRow:
  final class OfPure[S[_]] extends SubRow[Void, S]:
    def on(fx: Effects): fx.Sub[Void, S] = fx.subPure[S]
  final class OfOne[E[_], S[_]](e: InRow[E, S]) extends SubRow[E, S]:
    def on(fx: Effects): fx.Sub[E, S] = fx.subOne[E, S](using e.on(fx))
  final class OfMore[R[_], E[_], S[_]](r: SubRow[R, S], e: InRow[E, S]) extends SubRow[[x] =>> Sum[R, E, x], S]:
    def on(fx: Effects): fx.Sub[[x] =>> Sum[R, E, x], S] = fx.subMore[R, E, S](using r.on(fx), e.on(fx))


/**
 * TAGLESS, AS SCALA 3 WRITES IT: THE INTERFACE OVER NO IMPLEMENTATION, an `Effects` itself —
 * `import okay.effects.tagless.Tagless.*`, `import Tagless.std.*`. A program is a `Program`, a function of the
 * implementation; the evidence too. Each word makes, on the implementation given, that implementation's own: `pure`
 * its `pure`, a handler its `handle` of the same shape — so what an implementation does better it does here too. The
 * implementation is asked for once, where a program is run: `given Runner = Machine; run(p)`. Every word `inline`.
 *
 * ONE TRUSTED STEP: a `k` the implementation gives a general handler's clause is its own program, given back to the
 * clause as a `Program` (`resumed`) — run only on that implementation, by the types of the run it is in.
 */
object Tagless extends Effects:
  type ![+A, R[_]] = Program[A, R]
  type In[E[_], R[_]] = InRow[E, R]
  type Split[E[_], R[_], Rest[_]] = SplitRow[E, R, Rest]
  type Sub[R[_], S[_]] = SubRow[R, S]

  implicit inline def splitOne[E[_]]: Split[E, E, Pure] = InRow.One[E]()
  implicit inline def splitLast[R[_], E[_]]: Split[E, R + E, R] = InRow.Last[R, E]()
  implicit inline def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    InRow.Init[E, R, G, Rest](s)
  implicit inline def subPure[S[_]]: Sub[Pure, S] = SubRow.OfPure[S]()
  implicit inline def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = SubRow.OfOne[E, S](e)
  implicit inline def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = SubRow.OfMore[R, E, S](r, e)

  inline def pure[A, R[_]](a: A): A ! R = Program.Pure[A, R](a)
  inline def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Program.Bind[A, B, R](p, f)
  override inline def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = Program.Mapped[A, B, R](p, f)
  /** the program `p`, built when the run gets to it — on each implementation it is run on, anew */
  inline def delay[A, R[_]](p: => A ! R): A ! R = Program.Of[A, R](fx => fx.delay[A, R](p.on(fx)))
  protected[effects] inline def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = Program.Performed[E, R, X](op, in)

  inline def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E) =
    Program.Of[A, Rest + E](fx => fx.split[E, R, Rest, A](p.on(fx))(using s.on(fx)))
  inline def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S =
    Program.Of[A, S](fx => fx.embed[R, S, A](p.on(fx))(using s.on(fx)))

  /** the program run on the implementation in the implicit context */
  inline def run[A](p: A ! Pure)(using fx: Runner): A = fx.run[A](p.on(fx))
  /** a program whose one effect is `Lift[G, *]` as a `G[A]`, on the implementation in the implicit context */
  inline def lower[G[_], A](p: A ! ([x] =>> Std.Lift[G, x]))(using G: Std.Lowering[G], fx: Runner): G[A] =
    fx.lower[G, A](p.on(fx))
  /** tagless run on `fx`: a `Runner`, for code that runs programs over the interface */
  inline def on(fx: Runner): TaglessRunner = TaglessRunner(fx)

  /** THE TRUSTED STEP: `fx`'s own program, a `k`'s, as a `Program` — its run on `fx`, the one it was made on. On
   * another implementation it cannot be run: a `k` kept past its handler's run and run on another */
  private[tagless] def resumed[B, R[_]](fx: Effects)(p: fx.![B, R]): B ! R =
    Program.Of[B, R](other =>
      if other eq fx then p.asInstanceOf[other.![B, R]]
      else throw IllegalStateException("a k of one implementation run on another"))

  /** a handler's clauses as `fx`'s: each clause's program made on `fx`, a `k` given back as a `Program`. Not inline:
   * one class of each, not one at each use */
  private[tagless] def general[E[_], R[_], A, B](fx: Effects)(h: Handler[E, R, A, B]): fx.Handler[E, R, A, B] =
    new Clauses[E, A, fx.![B, R]]:
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def op[X](op: E[X], k: X => fx.![B, R]): fx.![B, R] = h.op[X](op, (x: X) => resumed[B, R](fx)(k(x))).on(fx)
  private[tagless] def tail[E[_], R[_], A, B](fx: Effects)(h: TailHandler[E, R, A, B]): fx.TailHandler[E, R, A, B] =
    new TailClauses[E, A, fx.![B, R]]:
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def answer[X](op: E[X]): X = h.answer[X](op)
  private[tagless] def aborting[E[_], R[_], A, B](fx: Effects)(h: AbortHandler[E, R, A, B]): fx.AbortHandler[E, R, A, B] =
    new AbortClauses[E, A, fx.![B, R]]:
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def abort[X](op: E[X]): fx.![B, R] = h.abort[X](op).on(fx)
  private[tagless] def statefulTail[E[_], S, R[_], A, B](fx: Effects)(h: StatefulTailHandler[E, S, R, A, B]):
      fx.StatefulTailHandler[E, S, R, A, B] =
    new StatefulTailClauses[E, S, A, fx.![B, R]]:
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def answer[X](cell: okay.effects.Cell[S], op: E[X]): X = h.answer[X](cell, op)
  private[tagless] def stateful[E[_], S, R[_], A, B](fx: Effects)(h: StatefulHandler[E, S, R, A, B]): fx.StatefulHandler[E, S, R, A, B] =
    new StatefulClauses[E, S, A, fx.![B, R]]:
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def op[X](s: S, op: E[X], k: (X, S) => fx.![B, R]): fx.![B, R] =
        h.op[X](s, op, (x: X, s2: S) => resumed[B, R](fx)(k(x, s2))).on(fx)

  /** `E` handled from wherever it is in the row, by a handler of any of the five shapes: the implementation's
   * `handle` of that shape */
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), general[E, Rest, A, B](fx)(h))(using s.on(fx)))
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), tail[E, Rest, A, B](fx)(h))(using s.on(fx)))
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), aborting[E, Rest, A, B](fx)(h))(using s.on(fx)))
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, statefulTail[E, S, Rest, A, B](fx)(h))(using s.on(fx)))
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, stateful[E, S, Rest, A, B](fx)(h))(using s.on(fx)))

  /** `E`, the last of the row, handled: the implementation's `handleLast` */
  inline def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: Handler[E, R, A, B]): B ! R =
    Program.Of[B, R](fx => fx.handleLast[E, R, A, B](p.on(fx), general[E, R, A, B](fx)(h)))
