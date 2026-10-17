package okay.effects.tagless

import scala.annotation.targetName
import okay.effects.{AbortClauses, Clauses, Effects, StatefulClauses, StatefulTailClauses, Std, Sum, TailClauses, Void}

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

/** each implementation's derivations, made on it: in the implicit scope, no import needed */
object InRow:
  final class One[E[_]] extends SplitRow[E, E, Void]:
    def on(fx: Effects): fx.Split[E, E, Void] = fx.splitOne[E]
  final class Last[R[_], E[_]] extends SplitRow[E, [x] =>> Sum[R, E, x], R]:
    def on(fx: Effects): fx.Split[E, [x] =>> Sum[R, E, x], R] = fx.splitLast[R, E]
  final class Init[E[_], R[_], G[_], Rest[_]](s: SplitRow[E, R, Rest])
      extends SplitRow[E, [x] =>> Sum[R, G, x], [x] =>> Sum[Rest, G, x]]:
    def on(fx: Effects): fx.Split[E, [x] =>> Sum[R, G, x], [x] =>> Sum[Rest, G, x]] = fx.splitInit[E, R, G, Rest](using s.on(fx))

  inline given splitOne[E[_]]: SplitRow[E, E, Void] = One[E]()
  inline given splitLast[R[_], E[_]]: SplitRow[E, [x] =>> Sum[R, E, x], R] = Last[R, E]()
  inline given splitInit[E[_], R[_], G[_], Rest[_]](using s: SplitRow[E, R, Rest]):
      SplitRow[E, [x] =>> Sum[R, G, x], [x] =>> Sum[Rest, G, x]] = Init[E, R, G, Rest](s)

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

  inline given subPure[S[_]]: SubRow[Void, S] = OfPure[S]()
  inline given subOne[E[_], S[_]](using e: InRow[E, S]): SubRow[E, S] = OfOne[E, S](e)
  inline given subMore[R[_], E[_], S[_]](using r: SubRow[R, S], e: InRow[E, S]): SubRow[[x] =>> Sum[R, E, x], S] = OfMore[R, E, S](r, e)

/** `perform[E]`'s rest: the operation, its answer `X` inferred from it, the row `R` from what is expected or given */
final class PerformOf[E[_]](val unused: Boolean) extends AnyVal:
  inline def apply[R[_], X](op: E[X])(using in: InRow[E, R]): Program[X, R] =
    Program.Performed[E, R, X](op, in)

/**
 * TAGLESS, AS SCALA 3 WRITES IT: THE INTERFACE'S WORDS OVER NO IMPLEMENTATION — `import okay.effects.tagless.Tagless.*`,
 * `import Tagless.std.*`. A program is a `Program`, a function of the implementation; the evidence too. Each word
 * makes, on the implementation given, that implementation's own: `pure` its `pure`, a handler its `handle` of the same
 * shape, a standard handler its `std`'s — so what an implementation does better it does here too. The implementation
 * is asked for once, where a program is run: `given Effects = Machine; run(p)`. Every word `inline`.
 *
 * ONE TRUSTED STEP: a `k` the implementation gives a general handler's clause is its own program, given back to the
 * clause as a `Program` (`resumed`) — run only on that implementation, by the types of the run it is in.
 */
object Tagless:
  type ![+A, R[_]] = Program[A, R]
  type Pure[X] = Void[X]
  type In[E[_], R[_]] = InRow[E, R]
  type Split[E[_], R[_], Rest[_]] = SplitRow[E, R, Rest]
  type Sub[R[_], S[_]] = SubRow[R, S]
  type +[R[_], E[_]] = [X] =>> Sum[R, E, X]
  type %[F[_, _], S] = okay.effects.%[F, S]
  type *[F[_]] = okay.effects.*[F]
  /** a context bound: `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R` */
  type Has[E[_]] = [R[_]] =>> In[E, R]

  inline def pure[A, R[_]](a: A): A ! R = Program.Pure[A, R](a)
  inline def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Program.Bind[A, B, R](p, f)
  inline def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = Program.Mapped[A, B, R](p, f)
  /** the program `p`, built when the run gets to it — on each implementation it is run on, anew */
  inline def delay[A, R[_]](p: => A ! R): A ! R = Program.Of[A, R](fx => fx.delay[A, R](p.on(fx)))

  /** an operation of `E`: `perform[Choose](Choose.Among(xs))` */
  inline def perform[E[_]]: PerformOf[E] = PerformOf[E](false)

  inline def split[E[_], R[_], Rest[_], A](p: A ! R)(using s: Split[E, R, Rest]): A ! (Rest + E) =
    Program.Of[A, Rest + E](fx => fx.split[E, R, Rest, A](p.on(fx))(using s.on(fx)))
  inline def embed[R[_], S[_], A](p: A ! R)(using s: Sub[R, S]): A ! S =
    Program.Of[A, S](fx => fx.embed[R, S, A](p.on(fx))(using s.on(fx)))

  /** tagless as an implementation of the interface, on `fx`: code written over `Effects` runs on it */
  inline def on(fx: Effects): TaglessEffects = TaglessEffects(fx)

  /** the program run on the implementation in the implicit context */
  inline def run[A](p: A ! Pure)(using fx: Effects): A = fx.run[A](p.on(fx))

  extension [A, R[_]](p: A ! R)
    @targetName("programFlatMap") inline def flatMap[B](f: A => B ! R): B ! R = Program.Bind[A, B, R](p, f)
    @targetName("programMap") inline def map[B](f: A => B): B ! R = Program.Mapped[A, B, R](p, f)
    inline def as[B](b: B): B ! R = Program.Mapped[A, B, R](p, _ => b)
    inline def andThen[B](q: => B ! R): B ! R = Program.Bind[A, B, R](p, _ => q)
    inline def into[S[_]](using s: Sub[R, S]): A ! S = embed[R, S, A](p)

  type Handler[E[_], R[_], A, B] = Clauses[E, A, B ! R]
  type TailHandler[E[_], R[_], A, B] = TailClauses[E, A, B ! R]
  type AbortHandler[E[_], R[_], A, B] = AbortClauses[E, A, B ! R]
  type StatefulTailHandler[E[_], S, R[_], A, B] = StatefulTailClauses[E, S, A, B ! R]
  type StatefulHandler[E[_], S, R[_], A, B] = StatefulClauses[E, S, A, B ! R]
  type Cell[S] = okay.effects.Cell[S]

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
  inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(using s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), general[E, Rest, A, B](fx)(h))(using s.on(fx)))
  inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(using s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), tail[E, Rest, A, B](fx)(h))(using s.on(fx)))
  inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(using s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, R, Rest, A, B](p.on(fx), aborting[E, Rest, A, B](fx)(h))(using s.on(fx)))
  inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      using s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, statefulTail[E, S, Rest, A, B](fx)(h))(using s.on(fx)))
  inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
      using s: Split[E, R, Rest]): B ! Rest =
    Program.Of[B, Rest](fx => fx.handle[E, S, R, Rest, A, B](p.on(fx), s0, stateful[E, S, Rest, A, B](fx)(h))(using s.on(fx)))

  /** `E`, the last of the row, handled: the implementation's `handleLast` */
  inline def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: Handler[E, R, A, B]): B ! R =
    Program.Of[B, R](fx => fx.handleLast[E, R, A, B](p.on(fx), general[E, R, A, B](fx)(h)))

  /** THE STANDARD EFFECTS: `import Tagless.std.*` — the operations made here, the handlers each implementation's own
   * `std`'s */
  object std:
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

    inline def get[S, R[_]](using in: In[State % S, R]): S ! R = perform[State % S][R, S](Std.State.Get(s => s))
    inline def put[S, R[_]](s: S)(using in: In[State % S, R]): Unit ! R = perform[State % S][R, Unit](Std.State.Put(s, u => u))
    inline def modify[S, R[_]](f: S => S)(using in: In[State % S, R]): Unit ! R =
      Program.Bind[S, Unit, R](get[S, R], (s: S) => put[S, R](f(s)))
    inline def ask[E, R[_]](using in: In[Reader % E, R]): E ! R = perform[Reader % E][R, E](Std.Reader.Ask(e => e))
    inline def tell[W, R[_]](w: W)(using in: In[Writer % W, R]): Unit ! R = perform[Writer % W][R, Unit](Std.Writer.Tell(w, u => u))
    inline def raise[E, R[_], X](e: E)(using in: In[Error % E, R]): X ! R = perform[Error % E][R, X](Std.Error.Raise(e))
    inline def among[X, R[_]](xs: List[X])(using in: In[Choose, R]): X ! R = perform[Choose][R, X](Std.Choose.Among(xs))
    inline def fail[X, R[_]](using in: In[Choose, R]): X ! R = among[X, R](Nil)
    inline def lift[G[_], X, R[_]](ga: G[X])(using in: In[[x] =>> Lift[G, x], R]): X ! R = perform[[x] =>> Lift[G, x]][R, X](Std.Lift.Of(ga))
    inline def emit[O, R[_]](o: O)(using in: In[Yield % O, R]): Unit ! R = perform[Yield % O][R, Unit](Std.Yield.Emit(o, u => u))

    /** the final state and the value: the implementation's `state` */
    inline def state[S](s0: S): StateHandler[S] = StateHandler(s0)
    final class StateHandler[S](val s0: S):
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[State % S, R, Rest]): (S, A) ! Rest =
        Program.Of[(S, A), Rest](fx => fx.std.state[S](s0).apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def reader[E](e: E): ReaderHandler[E] = ReaderHandler(e)
    final class ReaderHandler[E](val e: E):
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[Reader % E, R, Rest]): A ! Rest =
        Program.Of[A, Rest](fx => fx.std.reader[E](e).apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def writer[W]: WriterHandler[W] = WriterHandler[W]()
    final class WriterHandler[W]:
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[Writer % W, R, Rest]): (List[W], A) ! Rest =
        Program.Of[(List[W], A), Rest](fx => fx.std.writer[W].apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def attempt[E]: ErrorHandler[E] = ErrorHandler[E]()
    final class ErrorHandler[E]:
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[Error % E, R, Rest]): Either[E, A] ! Rest =
        Program.Of[Either[E, A], Rest](fx => fx.std.attempt[E].apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def recover[E]: RecoverHandler[E] = RecoverHandler[E]()
    final class RecoverHandler[E]:
      inline def apply[R[_], Rest[_], A](p: A ! R)(h: E => A ! Rest)(using sp: Split[Error % E, R, Rest]): A ! Rest =
        Program.Of[A, Rest](fx => fx.std.recover[E].apply[R, Rest, A](p.on(fx))((e: E) => h(e).on(fx))(using sp.on(fx)))
    inline def collect[O]: CollectHandler[O] = CollectHandler[O]()
    final class CollectHandler[O]:
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[Yield % O, R, Rest]): (List[O], A) ! Rest =
        Program.Of[(List[O], A), Rest](fx => fx.std.collect[O].apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def each[O]: EachHandler[O] = EachHandler[O]()
    final class EachHandler[O]:
      inline def apply[R[_], Rest[_], A](p: A ! R)(f: O => Unit ! Rest)(using sp: Split[Yield % O, R, Rest]): A ! Rest =
        Program.Of[A, Rest](fx => fx.std.each[O].apply[R, Rest, A](p.on(fx))((o: O) => f(o).on(fx))(using sp.on(fx)))
    inline def take[O](n: Int): TakeHandler[O] = TakeHandler[O](n)
    final class TakeHandler[O](val n: Int):
      inline def apply[R[_], Rest[_], A](p: A ! R)(using sp: Split[Yield % O, R, Rest]): (List[O], Option[A]) ! Rest =
        Program.Of[(List[O], Option[A]), Rest](fx => fx.std.take[O](n).apply[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def first[R[_], Rest[_], A](p: A ! R)(using sp: Split[Choose, R, Rest]): Option[A] ! Rest =
      Program.Of[Option[A], Rest](fx => fx.std.first[R, Rest, A](p.on(fx))(using sp.on(fx)))
    inline def all[R[_], Rest[_], A](p: A ! R)(using sp: Split[Choose, R, Rest]): List[A] ! Rest =
      Program.Of[List[A], Rest](fx => fx.std.all[R, Rest, A](p.on(fx))(using sp.on(fx)))
    /** a program whose one effect is `Lift[G, *]` as a `G[A]`, on the implementation in the implicit context */
    inline def lower[G[_], A](p: A ! ([x] =>> Lift[G, x]))(using G: Lowering[G], fx: Effects): G[A] = fx.std.lower[G, A](p.on(fx))

    /** `shift`, `reset`: of this program type, its `Control` — the answer program a `Program` */
    inline def shift[A, B, R[_], Rr[_]](f: (A => B ! R) => B ! R)(using in: In[Control % (B ! R), Rr]): A ! Rr =
      perform[Control % (B ! R)][Rr, A](Std.Control.Shift[B ! R, A](f))
    inline def reset[B, R[_]](p: B ! (R + (Control % (B ! R)))): B ! R =
      handleLast[Control % (B ! R), R, B, B](p, Resetting[B, R]())
    final class Resetting[B, R[_]] extends Clauses[Control % (B ! R), B, B ! R]:
      def ret(b: B): B ! R = Program.Pure[B, R](b)
      def op[X](op: Control[B ! R, X], k: X => B ! R): B ! R = op match { case Std.Control.Shift(f) => f(k) }
