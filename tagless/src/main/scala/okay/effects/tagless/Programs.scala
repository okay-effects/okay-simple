package okay.effects.tagless

import okay.effects.{AbortClauses, Clauses, Effects, StatefulClauses, StatefulTailClauses, Std, Sum, TailClauses, Void}

/** A PROGRAM OF NO IMPLEMENTATION: made, when it is run, on the implementation given — `on(fx)`, `fx`'s own program.
 * A value: made once, run on any implementation */
trait Program[+A, R[_]] {
  def on(fx: Effects): fx.![A, R]
}

/** `E` is in `R`, on any implementation: `fx`'s own evidence, made on it */
trait InRow[E[_], R[_]] {
  def on(fx: Effects): fx.In[E, R]
}
/** `E` in `R` and `R` without it `Rest`, on any implementation */
trait SplitRow[E[_], R[_], Rest[_]] extends InRow[E, R] {
  def on(fx: Effects): fx.Split[E, R, Rest]
}
/** each implementation's derivations, made on it: in the implicit scope, no import needed */
object InRow {
  implicit def splitOne[E[_]]: SplitRow[E, E, Void] = new SplitRow[E, E, Void] {
    def on(fx: Effects): fx.Split[E, E, Void] = fx.splitOne[E]
  }
  implicit def splitLast[R[_], E[_]]: SplitRow[E, ({ type L[x] = Sum[R, E, x] })#L, R] =
    new SplitRow[E, ({ type L[x] = Sum[R, E, x] })#L, R] {
      def on(fx: Effects): fx.Split[E, ({ type L[x] = Sum[R, E, x] })#L, R] = fx.splitLast[R, E]
    }
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: SplitRow[E, R, Rest]):
      SplitRow[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] =
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
  implicit def subPure[S[_]]: SubRow[Void, S] = new SubRow[Void, S] {
    def on(fx: Effects): fx.Sub[Void, S] = fx.subPure[S]
  }
  implicit def subOne[E[_], S[_]](implicit e: InRow[E, S]): SubRow[E, S] = new SubRow[E, S] {
    def on(fx: Effects): fx.Sub[E, S] = fx.subOne[E, S](e.on(fx))
  }
  implicit def subMore[R[_], E[_], S[_]](implicit r: SubRow[R, S], e: InRow[E, S]): SubRow[({ type L[x] = Sum[R, E, x] })#L, S] =
    new SubRow[({ type L[x] = Sum[R, E, x] })#L, S] {
      def on(fx: Effects): fx.Sub[({ type L[x] = Sum[R, E, x] })#L, S] = fx.subMore[R, E, S](r.on(fx), e.on(fx))
    }
}

/** `perform[E]`'s rest: the operation, its answer `X` inferred from it, the row `R` from what is expected or given */
final class PerformOf[E[_]](val unused: Boolean) extends AnyVal {
  def apply[R[_], X](op: E[X])(implicit in: InRow[E, R]): Program[X, R] = new Program[X, R] {
    def on(fx: Effects): fx.![X, R] = fx.perform[E].apply[R, X](op)(in.on(fx))
  }
}

/**
 * TAGLESS: THE INTERFACE'S WORDS OVER NO IMPLEMENTATION. A program is a `Program`, a function of the implementation;
 * the evidence too. Each word makes, on the implementation given, that implementation's own: `pure` its `pure`, a
 * handler its `handle` of the same shape, a standard handler its `std`'s — so what an implementation does better
 * (an answer in place, a handler at its place, a `std` of its own) it does here too. The implementation is asked
 * for once, where a program is run: `run(p)` with an `Effects` in the implicit context.
 *
 * ONE TRUSTED STEP: a `k` the implementation gives a general handler's clause is its own program, given back to the
 * clause as a `Program` (`resumed`) — run only on that implementation, by the types of the run it is in.
 */
abstract class Programs {
  type ![+A, R[_]] = Program[A, R]
  type Pure[X] = Void[X]
  type In[E[_], R[_]] = InRow[E, R]
  type Split[E[_], R[_], Rest[_]] = SplitRow[E, R, Rest]
  type Sub[R[_], S[_]] = SubRow[R, S]

  def pure[A, R[_]](a: A): A ! R = new Program[A, R] {
    def on(fx: Effects): fx.![A, R] = fx.pure[A, R](a)
  }
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = new Program[B, R] {
    def on(fx: Effects): fx.![B, R] = fx.flatMap[A, B, R](p.on(fx))((a: A) => f(a).on(fx))
  }
  def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = new Program[B, R] {
    def on(fx: Effects): fx.![B, R] = fx.map[A, B, R](p.on(fx))(f)
  }
  /** the program `p`, built when the run gets to it — on each implementation it is run on, anew */
  def delay[A, R[_]](p: => A ! R): A ! R = new Program[A, R] {
    def on(fx: Effects): fx.![A, R] = fx.delay[A, R](p.on(fx))
  }

  /** an operation of `E`: `perform[Choose](Choose.Among(xs))` */
  final def perform[E[_]]: PerformOf[E] = new PerformOf[E](false)

  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! ({ type L[x] = Sum[Rest, E, x] })#L =
    new Program[A, ({ type L[x] = Sum[Rest, E, x] })#L] {
      def on(fx: Effects): fx.![A, ({ type L[x] = Sum[Rest, E, x] })#L] = fx.split[E, R, Rest, A](p.on(fx))(s.on(fx))
    }
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = new Program[A, S] {
    def on(fx: Effects): fx.![A, S] = fx.embed[R, S, A](p.on(fx))(s.on(fx))
  }

  /** the program run on the implementation in the implicit context */
  def run[A](p: A ! Pure)(implicit fx: Effects): A = fx.run[A](p.on(fx))

  type Handler[E[_], R[_], A, B] = Clauses[E, A, B ! R]
  type TailHandler[E[_], R[_], A, B] = TailClauses[E, A, B ! R]
  type AbortHandler[E[_], R[_], A, B] = AbortClauses[E, A, B ! R]
  type StatefulTailHandler[E[_], S, R[_], A, B] = StatefulTailClauses[E, S, A, B ! R]
  type StatefulHandler[E[_], S, R[_], A, B] = StatefulClauses[E, S, A, B ! R]
  type Cell[S] = okay.effects.Cell[S]

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
  private def statefulTail[E[_], S, R[_], A, B](fx: Effects)(h: StatefulTailHandler[E, S, R, A, B]):
      fx.StatefulTailHandler[E, S, R, A, B] =
    new StatefulTailClauses[E, S, A, fx.![B, R]] {
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def answer[X](cell: okay.effects.Cell[S], op: E[X]): X = h.answer[X](cell, op)
    }
  private def stateful[E[_], S, R[_], A, B](fx: Effects)(h: StatefulHandler[E, S, R, A, B]): fx.StatefulHandler[E, S, R, A, B] =
    new StatefulClauses[E, S, A, fx.![B, R]] {
      def ret(s: S, a: A): fx.![B, R] = h.ret(s, a).on(fx)
      def op[X](s: S, op: E[X], k: (X, S) => fx.![B, R]): fx.![B, R] =
        h.op[X](s, op, (x: X, s2: S) => resumed[B, R](fx)(k(x, s2))).on(fx)
    }

  /** `E` handled from wherever it is in the row, by a handler of any of the five shapes: the implementation's
   * `handle` of that shape */
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] = fx.handle[E, R, Rest, A, B](p.on(fx), general[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] = fx.handle[E, R, Rest, A, B](p.on(fx), tail[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] = fx.handle[E, R, Rest, A, B](p.on(fx), aborting[E, Rest, A, B](fx)(h))(s.on(fx))
    }
  def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    new Program[B, Rest] {
      def on(fx: Effects): fx.![B, Rest] =
        fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, statefulTail[E, S, Rest, A, B](fx)(h))(s.on(fx))
    }
  def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
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

  /** THE STANDARD EFFECTS: `import Tagless.std._` — the operations made here, the handlers each implementation's
   * own `std`'s */
  final val std: TaglessStd = new TaglessStd

  final class TaglessStd {
    type State[S, X] = Std.State[S, X]
    val State: Std.State.type = Std.State
    type Reader[E, X] = Std.Reader[E, X]
    val Reader: Std.Reader.type = Std.Reader
    type Writer[W, X] = Std.Writer[W, X]
    val Writer: Std.Writer.type = Std.Writer
    type Error[E, X] = Std.Error[E, X]
    val Error: Std.Error.type = Std.Error
    type Choose[X] = Std.Choose[X]
    val Choose: Std.Choose.type = Std.Choose
    type Control[P, X] = Std.Control[P, X]
    val Control: Std.Control.type = Std.Control
    type Yield[O, X] = Std.Yield[O, X]
    val Yield: Std.Yield.type = Std.Yield
    type Lift[G[_], X] = Std.Lift[G, X]
    val Lift: Std.Lift.type = Std.Lift
    type Lowering[G[_]] = Std.Lowering[G]

    def get[S, R[_]](implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R =
      perform[({ type L[x] = State[S, x] })#L].apply[R, S](Std.State.Get(s => s))
    def put[S, R[_]](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R =
      perform[({ type L[x] = State[S, x] })#L].apply[R, Unit](Std.State.Put(s, u => u))
    def modify[S, R[_]](f: S => S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R =
      flatMap(get[S, R])((s: S) => put[S, R](f(s)))
    def ask[E, R[_]](implicit in: In[({ type L[x] = Reader[E, x] })#L, R]): E ! R =
      perform[({ type L[x] = Reader[E, x] })#L].apply[R, E](Std.Reader.Ask(e => e))
    def tell[W, R[_]](w: W)(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R =
      perform[({ type L[x] = Writer[W, x] })#L].apply[R, Unit](Std.Writer.Tell(w, u => u))
    def raise[E, R[_], X](e: E)(implicit in: In[({ type L[x] = Error[E, x] })#L, R]): X ! R =
      perform[({ type L[x] = Error[E, x] })#L].apply[R, X](Std.Error.Raise(e))
    def among[X, R[_]](xs: List[X])(implicit in: In[Choose, R]): X ! R = perform[Choose].apply[R, X](Std.Choose.Among(xs))
    def fail[X, R[_]](implicit in: In[Choose, R]): X ! R = among[X, R](Nil)
    def lift[G[_], X, R[_]](ga: G[X])(implicit in: In[({ type L[x] = Lift[G, x] })#L, R]): X ! R =
      perform[({ type L[x] = Lift[G, x] })#L].apply[R, X](Std.Lift.Of(ga))
    def emit[O, R[_]](o: O)(implicit in: In[({ type L[x] = Yield[O, x] })#L, R]): Unit ! R =
      perform[({ type L[x] = Yield[O, x] })#L].apply[R, Unit](Std.Yield.Emit(o, u => u))

    /** the final state and the value: the implementation's `state` */
    def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
    final class StateHandler[S](s0: S) {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = State[S, x] })#L, R, Rest]): (S, A) ! Rest =
        new Program[(S, A), Rest] {
          def on(fx: Effects): fx.![(S, A), Rest] = fx.std.state[S](s0).apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def reader[E](e: E): ReaderHandler[E] = new ReaderHandler(e)
    final class ReaderHandler[E](e: E) {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Reader[E, x] })#L, R, Rest]): A ! Rest =
        new Program[A, Rest] {
          def on(fx: Effects): fx.![A, Rest] = fx.std.reader[E](e).apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def writer[W]: WriterHandler[W] = new WriterHandler[W]
    final class WriterHandler[W] {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Writer[W, x] })#L, R, Rest]): (List[W], A) ! Rest =
        new Program[(List[W], A), Rest] {
          def on(fx: Effects): fx.![(List[W], A), Rest] = fx.std.writer[W].apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
    final class ErrorHandler[E] {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): Either[E, A] ! Rest =
        new Program[Either[E, A], Rest] {
          def on(fx: Effects): fx.![Either[E, A], Rest] = fx.std.attempt[E].apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def recover[E]: RecoverHandler[E] = new RecoverHandler[E]
    final class RecoverHandler[E] {
      def apply[R[_], Rest[_], A](p: A ! R)(h: E => A ! Rest)(implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): A ! Rest =
        new Program[A, Rest] {
          def on(fx: Effects): fx.![A, Rest] = fx.std.recover[E].apply[R, Rest, A](p.on(fx))((e: E) => h(e).on(fx))(sp.on(fx))
        }
    }
    def collect[O]: CollectHandler[O] = new CollectHandler[O]
    final class CollectHandler[O] {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], A) ! Rest =
        new Program[(List[O], A), Rest] {
          def on(fx: Effects): fx.![(List[O], A), Rest] = fx.std.collect[O].apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def each[O]: EachHandler[O] = new EachHandler[O]
    final class EachHandler[O] {
      def apply[R[_], Rest[_], A](p: A ! R)(f: O => Unit ! Rest)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): A ! Rest =
        new Program[A, Rest] {
          def on(fx: Effects): fx.![A, Rest] = fx.std.each[O].apply[R, Rest, A](p.on(fx))((o: O) => f(o).on(fx))(sp.on(fx))
        }
    }
    def take[O](n: Int): TakeHandler[O] = new TakeHandler[O](n)
    final class TakeHandler[O](n: Int) {
      def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], Option[A]) ! Rest =
        new Program[(List[O], Option[A]), Rest] {
          def on(fx: Effects): fx.![(List[O], Option[A]), Rest] = fx.std.take[O](n).apply[R, Rest, A](p.on(fx))(sp.on(fx))
        }
    }
    def first[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): Option[A] ! Rest = new Program[Option[A], Rest] {
      def on(fx: Effects): fx.![Option[A], Rest] = fx.std.first[R, Rest, A](p.on(fx))(sp.on(fx))
    }
    def all[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest = new Program[List[A], Rest] {
      def on(fx: Effects): fx.![List[A], Rest] = fx.std.all[R, Rest, A](p.on(fx))(sp.on(fx))
    }
    /** a program whose one effect is `Lift[G, *]` as a `G[A]`, on the implementation in the implicit context */
    def lower[G[_], A](p: A ! ({ type L[x] = Lift[G, x] })#L)(implicit G: Lowering[G], fx: Effects): G[A] =
      fx.std.lower[G, A](p.on(fx))

    /** `shift`, `reset`: of this program type, its `Control` — the answer program a `Program` */
    def shift[A, B, R[_], Rr[_]](f: (A => B ! R) => B ! R)(implicit in: In[({ type L[x] = Control[B ! R, x] })#L, Rr]): A ! Rr =
      perform[({ type L[x] = Control[B ! R, x] })#L].apply[Rr, A](Std.Control.Shift[B ! R, A](f))
    def reset[B, R[_]](p: B ! ({ type L[x] = Sum[R, ({ type M[y] = Control[B ! R, y] })#M, x] })#L): B ! R =
      handleLast[({ type L[x] = Control[B ! R, x] })#L, R, B, B](p, new Clauses[({ type L[x] = Control[B ! R, x] })#L, B, B ! R] {
        def ret(b: B): B ! R = pure[B, R](b)
        def op[X](op: Control[B ! R, X], k: X => B ! R): B ! R = op match { case Std.Control.Shift(f) => f(k) }
      })
  }
}
