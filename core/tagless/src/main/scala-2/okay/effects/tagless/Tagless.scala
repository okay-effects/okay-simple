package okay.effects.tagless

import okay.effects.{
  AbortClauses, Clauses, Effects, Runner, StatefulClauses, StatefulTailClauses, Std, Sum, TailClauses, Void
}

/** A PROGRAM OF NO IMPLEMENTATION: made, when it is run, on the implementation given — `on(fx)`, `fx`'s own program.
 * A value: made once, run on any implementation */
abstract class Program[+A, R[_]] {
  def on(fx: Effects): fx.![A, R]
  /** made on `fx` as the `depth`-th of a chain: a bind's or a map's own; any other, `on` */
  private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] = on(fx)
}

object Program {
  /** a chain of binds and maps made by nested calls, so many deep at most; one deeper `delay`ed, made by `fx`'s run —
   * a loop — when it gets there: a chain of any length in constant stack, one `delay` in so many */
  private final val Deepest = 64

  final class Pure[A, R[_]](a: A) extends Program[A, R] {
    def on(fx: Effects): fx.![A, R] = fx.pure[A, R](a)
  }
  final class Bind[X, A, R[_]](p: Program[X, R], f: X => Program[A, R]) extends Program[A, R] {
    def on(fx: Effects): fx.![A, R] = onAt(fx, 0)
    override private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] =
      fx.flatMap[X, A, R](before[X, R](fx, p, depth))((x: X) => f(x).on(fx))
  }
  final class Mapped[X, A, R[_]](p: Program[X, R], f: X => A) extends Program[A, R] {
    def on(fx: Effects): fx.![A, R] = onAt(fx, 0)
    override private[tagless] def onAt(fx: Effects, depth: Int): fx.![A, R] =
      fx.map[X, A, R](before[X, R](fx, p, depth))(f)
  }
  /** the program before a bind or a map, made on `fx`: one deeper, or at `Deepest` `delay`ed and from 0 again */
  @inline private def before[X, R[_]](fx: Effects, p: Program[X, R], depth: Int): fx.![X, R] =
    if (depth < Deepest) p.onAt(fx, depth + 1) else fx.delay[X, R](p.onAt(fx, 0))
}

/** `E` is in `R`, on any implementation: `fx`'s own evidence, made on it */
trait InRow[E[_], R[_]] {
  def on(fx: Effects): fx.In[E, R]
}
/** `E` in `R` and `R` without it `Rest`, on any implementation */
trait SplitRow[E[_], R[_], Rest[_]] extends InRow[E, R] {
  def on(fx: Effects): fx.Split[E, R, Rest]
}
/** each implementation's derivations, made on it */
object InRow {
  def splitOne[E[_]]: SplitRow[E, E, Void] = new SplitRow[E, E, Void] {
    def on(fx: Effects): fx.Split[E, E, Void] = fx.splitOne[E]
  }
  def splitLast[R[_], E[_]]: SplitRow[E, ({ type L[x] = Sum[R, E, x] })#L, R] =
    new SplitRow[E, ({ type L[x] = Sum[R, E, x] })#L, R] {
      def on(fx: Effects): fx.Split[E, ({ type L[x] = Sum[R, E, x] })#L, R] = fx.splitLast[R, E]
    }
  def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: SplitRow[E, R, Rest])
    : SplitRow[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] =
    new SplitRow[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] {
      def on(fx: Effects): fx.Split[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] =
        fx.splitInit[E, R, G, Rest](s.on(fx))
    }
}

/** every effect of `R` is in `S`, on any implementation */
trait SubRow[R[_], S[_]] {
  def on(fx: Effects): fx.Sub[R, S]
}
object SubRow {
  def subPure[S[_]]: SubRow[Void, S] = new SubRow[Void, S] {
    def on(fx: Effects): fx.Sub[Void, S] = fx.subPure[S]
  }
  def subOne[E[_], S[_]](implicit e: InRow[E, S]): SubRow[E, S] = new SubRow[E, S] {
    def on(fx: Effects): fx.Sub[E, S] = fx.subOne[E, S](e.on(fx))
  }
  def subMore[R[_], E[_], S[_]](implicit r: SubRow[R, S], e: InRow[E, S]): SubRow[({ type L[x] = Sum[R, E, x] })#L, S] =
    new SubRow[({ type L[x] = Sum[R, E, x] })#L, S] {
      def on(fx: Effects): fx.Sub[({ type L[x] = Sum[R, E, x] })#L, S] = fx.subMore[R, E, S](r.on(fx), e.on(fx))
    }
}

/**
 * TAGLESS, AS SCALA 2 WRITES IT: THE INTERFACE OVER NO IMPLEMENTATION, an `Effects` itself —
 * `import okay.effects.tagless.Tagless._`, `import Tagless.std._`. A program is a `Program`, a function of the
 * implementation; the evidence too. Each word makes, on the implementation given, that implementation's own: `pure`
 * its `pure`, a handler its `handle` of the same shape — so what an implementation does better (an answer in place,
 * a handler at its place) it does here too. The implementation is asked for once, where a program is run: `run(p)`
 * with a `Runner` in the implicit context.
 *
 * ONE TRUSTED STEP: a `k` the implementation gives a general handler's clause is its own program, given back to the
 * clause as a `Program` (`resumed`) — run only on that implementation, by the types of the run it is in.
 */
object Tagless extends Effects {
  type ![+A, R[_]] = Program[A, R]
  type In[E[_], R[_]] = InRow[E, R]
  type Split[E[_], R[_], Rest[_]] = SplitRow[E, R, Rest]
  type Sub[R[_], S[_]] = SubRow[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = InRow.splitOne[E]
  implicit def splitLast[R[_], E[_]]: Split[E, ({ type L[x] = Sum[R, E, x] })#L, R] = InRow.splitLast[R, E]
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](
    implicit s: Split[E, R, Rest]): Split[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] =
    InRow.splitInit[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = SubRow.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = SubRow.subOne[E, S](e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[({ type L[x] = Sum[R, E, x] })#L, S] =
    SubRow.subMore[R, E, S](r, e)

  def pure[A, R[_]](a: A): A ! R = new Program.Pure[A, R](a)
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = new Program.Bind[A, B, R](p, f)
  override def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = new Program.Mapped[A, B, R](p, f)
  /** the program `p`, built when the run gets to it — on each implementation it is run on, anew */
  def delay[A, R[_]](p: => A ! R): A ! R = new Program[A, R] {
    def on(fx: Effects): fx.![A, R] = fx.delay[A, R](p.on(fx))
  }

  protected[effects] def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = new Program[X, R] {
    def on(fx: Effects): fx.![X, R] = fx.perform[E].apply[R, X](op)(in.on(fx))
  }

  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! ({ type L[x] = Sum[Rest, E, x] })#L =
    new Program[A, ({ type L[x] = Sum[Rest, E, x] })#L] {
      def on(fx: Effects): fx.![A, ({ type L[x] = Sum[Rest, E, x] })#L] = fx.split[E, R, Rest, A](p.on(fx))(s.on(fx))
    }
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = new Program[A, S] {
    def on(fx: Effects): fx.![A, S] = fx.embed[R, S, A](p.on(fx))(s.on(fx))
  }

  protected[effects] def interposed[E[_], R[_], A](p: A ! R)(
    implicit in: In[E, R]): A ! ({ type L[x] = Sum[R, E, x] })#L =
    new Program[A, ({ type L[x] = Sum[R, E, x] })#L] {
      def on(fx: Effects): fx.![A, ({ type L[x] = Sum[R, E, x] })#L] = fx.interposed[E, R, A](p.on(fx))(in.on(fx))
    }

  /** tagless run on `fx`: a `Runner`, for code that runs programs over the interface */
  def on(fx: Runner): TaglessRunner = new TaglessRunner(fx)

  /** the program run on the implementation in the implicit context */
  def run[A](p: A ! Pure)(implicit fx: Runner): A = fx.run[A](p.on(fx))
  /** a program whose one effect is `Lift[G, *]` as a `G[A]`, on the implementation in the implicit context */
  def lower[G[_], A](p: A ! ({ type L[x] = Std.Lift[G, x] })#L)(implicit G: Std.Lowering[G], fx: Runner): G[A] =
    fx.lower[G, A](p.on(fx))
  /** a program whose one effect left is `Lift[G, *]`, of any shape (`Pure + Lift[G, *]`), as a `G[A]` */
  def lowerLast[G[_], R[_], A](p: A ! R)(implicit s: Split[({ type L[x] = Std.Lift[G, x] })#L, R, Pure],
    G: Std.Lowering[G], fx: Runner): G[A] =
    fx.lowerLast[G, R, A](p.on(fx))(s.on(fx), G)

  /** THE TRUSTED STEP: `fx`'s own program, a `k`'s, as a `Program` — its run on `fx`, the one it was made on. On
   * another implementation it cannot be run: a `k` kept past its handler's run and run on another */
  private def resumed[B, R[_]](fx: Effects)(p: fx.![B, R]): B ! R = new Program[B, R] {
    def on(other: Effects): other.![B, R] =
      if (other eq fx) p.asInstanceOf[other.![B, R]]
      else throw new IllegalStateException("a k of one implementation run on another")
  }

  /** a handler's clauses as `fx`'s: each clause's program made on `fx`, a `k` given back as a `Program` */
  private def general[E[_], R[_], A, B](fx: Effects)(h: Handler[E, R, A, B]): fx.Handler[E, R, A, B] =
    new Clauses[E, A, fx.![B, R]] {
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def op[X](op: E[X], k: X => fx.![B, R]): fx.![B, R] = h.op[X](op, (x: X) => resumed[B, R](fx)(k(x))).on(fx)
    }
  private def tail[E[_], R[_], A, B](fx: Effects)(h: TailHandler[E, R, A, B]): fx.TailHandler[E, R, A, B] =
    new TailClauses[E, A, fx.![B, R]] {
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def answer[X](op: E[X]): X = h.answer[X](op)
    }
  private def aborting[E[_], R[_], A, B](fx: Effects)(h: AbortHandler[E, R, A, B]): fx.AbortHandler[E, R, A, B] =
    new AbortClauses[E, A, fx.![B, R]] {
      def ret(a: A): fx.![B, R] = h.ret(a).on(fx)
      def abort[X](op: E[X]): fx.![B, R] = h.abort[X](op).on(fx)
    }
  private def statefulTail[E[_], S, R[_], A, B](fx: Effects)(
    h: StatefulTailHandler[E, S, R, A, B]): fx.StatefulTailHandler[E, S, R, A, B] =
    new StatefulTailClauses[E, S, A, fx.![B, R]] {
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def answer[X](cell: okay.effects.Cell[S], op: E[X]): X = h.answer[X](cell, op)
    }
  private def stateful[E[_], S, R[_], A, B](fx: Effects)(
    h: StatefulHandler[E, S, R, A, B]): fx.StatefulHandler[E, S, R, A, B] =
    new StatefulClauses[E, S, A, fx.![B, R]] {
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def op[X](s: S, op: E[X], k: (X, S) => fx.![B, R]): fx.![B, R] =
        h.op[X](s, op, (x: X, s2: S) => resumed[B, R](fx)(k(x, s2))).on(fx)
    }

  /** `E` handled from wherever it is in the row, by a handler of any of the five shapes: the implementation's
   * `handle` of that shape */
  override def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] =
        fx.handle[E, R, Rest, A, B](p.on(fx), general[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  override def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] = fx.handle[E, R, Rest, A, B](p.on(fx), tail[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  override def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] =
        fx.handle[E, R, Rest, A, B](p.on(fx), aborting[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  override def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] =
        fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, statefulTail[E, S, Rest, A, B](fx)(h))(s.on(fx))
    }
  override def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] =
        fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, stateful[E, S, Rest, A, B](fx)(h))(s.on(fx))
    }

  /** `E`, the last of the row, handled: the implementation's `handleLast` */
  def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: Handler[E, R, A, B]): B ! R =
    new Program[B, R] {
      def on(fx: Effects): fx.![B, R] = fx.handleLast[E, R, A, B](p.on(fx), general[E, R, A, B](fx)(h))
    }
}
