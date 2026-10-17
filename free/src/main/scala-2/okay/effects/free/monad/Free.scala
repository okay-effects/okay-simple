package okay.effects.free.monad

import okay.effects.{Cell, Runner, Sum}
import scala.annotation.tailrec

/** THE `Free` MONAD, SCALA 2'S: a program is data — a value, an operation of `F`, a bind, an operation with its rest,
 * a program built later — and its handlers interpret it */
sealed abstract class Free[+F[_], +A] {
  /** bound: an operation with its rest one node at once, `Inject`; anything else a `Bind` */
  final def flatMap[G[x] >: F[x], B](f: A => Free[G, B]): Free[G, B] = this match {
    case Free.Perform(op) => Free.Inject[G, A, B](op, f)
    case _ => Free.Bind[G, A, B](this, f)
  }
  final def map[B](f: A => B): Free[F, B] = flatMap[F, B](a => Free.Return(f(a)))
}

/** an operation of the row `R`: its place in `R`, a number — the last effect's 0 — and the operation itself. Scala 2
 * has no GADT inference to type a place by the row, so a handler takes the operation at 0 as its own on trust: the
 * interface's evidence, found by the row's types, makes it so */
final case class At[R[_], X](at: Int, op: Any)

/**
 * THE MONAD'S HANDLERS, Scala 2's: a handler of `E`, the last of `R + E`, interprets the program from `R + E` to `R` —
 * a value to its value clause, an operation at place 0 to its clause, an operation further in passed on, one place
 * nearer. In constant stack: `@tailrec` over a bind of a value and binds nested to the left, and `k` a trampoline, the
 * rest given back `Delay`ed. Clauses are traits of a polymorphic `apply`, Scala 2 having no polymorphic functions.
 */
object Free {
  final case class Return[+A](a: A) extends Free[Nothing, A]
  final case class Perform[F[_], A](op: F[A]) extends Free[F, A]
  final case class Bind[F[_], X, A](m: Free[F, X], f: X => Free[F, A]) extends Free[F, A]
  /** an operation and its rest, one node: `Bind(Perform(op), k)`'s — what a handler passes an operation on as */
  final case class Inject[F[_], X, A](op: F[X], k: X => Free[F, A]) extends Free[F, A]
  /** the program `p()`, built when the run gets to it */
  final case class Delay[F[_], A](p: () => Free[F, A]) extends Free[F, A]

  /** a program of the value `A` over the row `R`: a `Free` of `R`'s operations */
  type ![+A, R[_]] = Free[({ type L[x] = At[R, x] })#L, A]

  /** a general handler's clause: the operation and `k` */
  trait Clause[E[_], P] { def apply[X](op: E[X], k: X => P): P }
  /** the same with a state of its own: the state of its time, and `k` resumed with the next */
  trait StateClause[E[_], S, P] { def apply[X](s: S, op: E[X], k: (X, S) => P): P }
  /** a clause answering in place, its state in `cell` */
  trait Answer[E[_], S] { def apply[X](cell: Cell[S], op: E[X]): X }

  /** THE TRUSTED STEP: an operation at place 0, of its handler's effect */
  private def opOf[E[_], X](op: Any): E[X] = op.asInstanceOf[E[X]]

  /** a program of no effect: its value */
  @tailrec def run[A](p: A ! okay.effects.Void): A = {
    type F[x] = At[okay.effects.Void, x]
    p match {
    case Return(a) => a
    case b: Bind[F, Any, A] @unchecked => b.m match {
      case Return(x) => run(b.f(x))
      case m: Bind[F, Any, Any] @unchecked => run(Bind(m.m, (y: Any) => Bind(m.f(y), b.f)))
      case d: Delay[F, Any] @unchecked => run(Bind(d.p(), b.f))
      case i: Inject[F, Any, Any] @unchecked => throw new IllegalStateException(s"an operation no handler took: ${i.op}")
      case Perform(o) => throw new IllegalStateException(s"an operation no handler took: $o")
    }
    case d: Delay[F, A] @unchecked => run(d.p())
    case i: Inject[F, Any, A] @unchecked => throw new IllegalStateException(s"an operation no handler took: ${i.op}")
    case Perform(o) => throw new IllegalStateException(s"an operation no handler took: $o")
  }
  }

  /** the program `p`, built when the run gets to it */
  def delay[A, R[_]](p: => A ! R): A ! R = Delay[({ type L[x] = At[R, x] })#L, A](() => p)

  /** `E`, the last of `R + E`, handled deeply */
  def handle[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L)(ret: A => B ! R, eff: Clause[E, B ! R]): B ! R =
    handling(p, ret, eff)

  @tailrec private def handling[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, ret: A => B ! R,
      eff: Clause[E, B ! R]): B ! R = {
    type F[x] = At[({ type L[y] = Sum[R, E, y] })#L, x]
    p match {
    case Return(a) => ret(a)
    case Perform(o) => on[E, R, Any, B](o, ret.asInstanceOf[Any => B ! R], eff)
    case j: Inject[F, Any, A] @unchecked => on[E, R, Any, B](j.op, (x: Any) => delay(handle(j.k(x))(ret, eff)), eff)
    case d: Delay[F, A] @unchecked => handling(d.p(), ret, eff)
    case b: Bind[F, Any, A] @unchecked => b.m match {
      case Return(x) => handling(b.f(x), ret, eff)
      case m: Bind[F, Any, Any] @unchecked => handling(Bind(m.m, (y: Any) => Bind(m.f(y), b.f)), ret, eff)
      case d: Delay[F, Any] @unchecked => handling(Bind(d.p(), b.f), ret, eff)
      case j: Inject[F, Any, Any] @unchecked => handling(Inject(j.op, (y: Any) => Bind(j.k(y), b.f)), ret, eff)
      case Perform(o) => on[E, R, Any, B](o, (v: Any) => delay(handle(b.f(v))(ret, eff)), eff)
    }
  }
  }

  /** an operation at `R + E`: at 0 `E`'s, to the clause; any other passed on, one place nearer */
  private def on[E[_], R[_], X, B](o: Any, k: X => B ! R, eff: Clause[E, B ! R]): B ! R = {
    val pl = o.asInstanceOf[At[Any, X]]
    if (pl.at == 0) eff[X](opOf[E, X](pl.op), k)
    else Inject[({ type L[x] = At[R, x] })#L, X, B](At[R, X](pl.at - 1, pl.op), k)
  }

  /** the same with a state of its own, from `s` */
  def handle[E[_], R[_], A, B, S](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s: S)(ret: (S, A) => B ! R,
      eff: StateClause[E, S, B ! R]): B ! R = stating(p, s, ret, eff)

  @tailrec private def stating[E[_], R[_], A, B, S](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s: S, ret: (S, A) => B ! R,
      eff: StateClause[E, S, B ! R]): B ! R = {
    type F[x] = At[({ type L[y] = Sum[R, E, y] })#L, x]
    p match {
    case Return(a) => ret(s, a)
    case Perform(o) => onWith[E, R, Any, B, S](o, s, (x: Any, s1: S) => ret(s1, x.asInstanceOf[A]), eff)
    case j: Inject[F, Any, A] @unchecked => onWith[E, R, Any, B, S](j.op, s, (x: Any, s1: S) => delay(handle(j.k(x), s1)(ret, eff)), eff)
    case d: Delay[F, A] @unchecked => stating(d.p(), s, ret, eff)
    case b: Bind[F, Any, A] @unchecked => b.m match {
      case Return(x) => stating(b.f(x), s, ret, eff)
      case m: Bind[F, Any, Any] @unchecked => stating(Bind(m.m, (y: Any) => Bind(m.f(y), b.f)), s, ret, eff)
      case d: Delay[F, Any] @unchecked => stating(Bind(d.p(), b.f), s, ret, eff)
      case j: Inject[F, Any, Any] @unchecked => stating(Inject(j.op, (y: Any) => Bind(j.k(y), b.f)), s, ret, eff)
      case Perform(o) => onWith[E, R, Any, B, S](o, s, (v: Any, s1: S) => delay(handle(b.f(v), s1)(ret, eff)), eff)
    }
  }
  }

  private def onWith[E[_], R[_], X, B, S](o: Any, s: S, k: (X, S) => B ! R, eff: StateClause[E, S, B ! R]): B ! R = {
    val pl = o.asInstanceOf[At[Any, X]]
    if (pl.at == 0) eff[X](s, opOf[E, X](pl.op), k)
    else Inject[({ type L[x] = At[R, x] })#L, X, B](At[R, X](pl.at - 1, pl.op), (x: X) => k(x, s))
  }

  /** `E`, the last of `R + E`, handled by a handler ANSWERING IN PLACE, from the state `s`: no `k`, the run going on
   * at once with the answer; where an operation is passed on, the rest from a cell of its own, of the state of its
   * time */
  def answer[E[_], R[_], A, B, S](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s: S)(ret: (S, A) => B ! R,
      ans: Answer[E, S]): B ! R = answering(p, new Cell.Var(s), ret, ans)

  @tailrec private def answering[E[_], R[_], A, B, S](p: A ! ({ type L[x] = Sum[R, E, x] })#L, cell: Cell.Var[S],
      ret: (S, A) => B ! R, ans: Answer[E, S]): B ! R = {
    type F[x] = At[({ type L[y] = Sum[R, E, y] })#L, x]
    p match {
    case Return(a) => ret(cell.state, a)
    case Perform(o) => answering(Inject(o, (a: A) => Return(a)), cell, ret, ans)
    case j: Inject[F, Any, A] @unchecked =>
      val pl = j.op.asInstanceOf[At[Any, Any]]
      if (pl.at == 0) answering(j.k(ans[Any](cell, opOf[E, Any](pl.op))), cell, ret, ans)
      else {
        val now = cell.state
        Inject[({ type L[x] = At[R, x] })#L, Any, B](At[R, Any](pl.at - 1, pl.op), (x: Any) => answer(j.k(x), now)(ret, ans))
      }
    case d: Delay[F, A] @unchecked => answering(d.p(), cell, ret, ans)
    case b: Bind[F, Any, A] @unchecked => b.m match {
      case Return(x) => answering(b.f(x), cell, ret, ans)
      case m: Bind[F, Any, Any] @unchecked => answering(Bind(m.m, (y: Any) => Bind(m.f(y), b.f)), cell, ret, ans)
      case d: Delay[F, Any] @unchecked => answering(Bind(d.p(), b.f), cell, ret, ans)
      case j: Inject[F, Any, Any] @unchecked => answering(Inject(j.op, (y: Any) => Bind(j.k(y), b.f)), cell, ret, ans)
      case Perform(o) => answering(Inject(o, b.f), cell, ret, ans)
    }
  }
  }

  /** `p`'s places renamed by `rename` */
  def remap[R1[_], R2[_], A](p: A ! R1, rename: Int => Int): A ! R2 = remapping(p, rename)

  @tailrec private def remapping[R1[_], R2[_], A](p: A ! R1, rename: Int => Int): A ! R2 = {
    type F[x] = At[R1, x]
    p match {
    case Return(a) => Return(a)
    case Perform(o) => remapping[R1, R2, A](Inject(o, (a: A) => Return(a)), rename)
    case j: Inject[F, Any, A] @unchecked =>
      val pl = j.op.asInstanceOf[At[Any, Any]]
      Inject[({ type L[x] = At[R2, x] })#L, Any, A](At[R2, Any](rename(pl.at), pl.op), (x: Any) => remap[R1, R2, A](j.k(x), rename))
    case d: Delay[F, A] @unchecked => Delay[({ type L[x] = At[R2, x] })#L, A](() => remap[R1, R2, A](d.p(), rename))
    case b: Bind[F, Any, A] @unchecked => b.m match {
      case Return(x) => remapping[R1, R2, A](b.f(x), rename)
      case m: Bind[F, Any, Any] @unchecked => remapping[R1, R2, A](Bind(m.m, (y: Any) => Bind(m.f(y), b.f)), rename)
      case d: Delay[F, Any] @unchecked => remapping[R1, R2, A](Bind(d.p(), b.f), rename)
      case j: Inject[F, Any, Any] @unchecked => remapping[R1, R2, A](Inject(j.op, (y: Any) => Bind(j.k(y), b.f)), rename)
      case Perform(o) => remapping[R1, R2, A](Inject(o, b.f), rename)
    }
  }
  }
}

/** THE INTERFACE ON THE `Free` MONAD, Scala 2's: its places numbers, as Scala 2's machine's */
class OnFree extends Runner {
  import Free.{Perform, Return}

  type ![+A, R[_]] = Free.![A, R]

  final class Place(val n: Int)
  type Split[E[_], R[_], Rest[_]] = Place
  type In[E[_], R[_]] = Place
  type Sub[R[_], S[_]] = Vector[Int]

  private val places: Array[Place] = Array.tabulate(64)(new Place(_))
  private def placeAt(i: Int): Place = if (i < places.length) places(i) else new Place(i)

  implicit def splitOne[E[_]]: Place = places(0)
  implicit def splitLast[R[_], E[_]]: Place = places(0)
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Place): Place = placeAt(s.n + 1)
  implicit def subPure[S[_]]: Vector[Int] = Vector()
  implicit def subOne[E[_], S[_]](implicit e: Place): Vector[Int] = Vector(e.n)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Vector[Int], e: Place): Vector[Int] = e.n +: r

  def pure[A, R[_]](a: A): A ! R = Return(a)
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = p.flatMap(f)
  def delay[A, R[_]](p: => A ! R): A ! R = Free.delay(p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: Place): X ! R =
    Perform[({ type L[x] = At[R, x] })#L, X](At[R, X](in.n, op))

  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Place): A ! ({ type L[x] = Sum[Rest, E, x] })#L = {
    val at = s.n
    if (at == 0) p.asInstanceOf[A ! ({ type L[x] = Sum[Rest, E, x] })#L]
    else Free.delay(Free.remap(p, (i: Int) => if (i == at) 0 else if (i < at) i + 1 else i))
  }
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Vector[Int]): A ! S = Free.delay(Free.remap(p, s))

  def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: Handler[E, R, A, B]): B ! R =
    Free.delay(Free.handle(p)((a: A) => h.ret(a), new Free.Clause[E, B ! R] {
      def apply[X](op: E[X], k: X => B ! R): B ! R = h.op[X](op, k)
    }))
  override def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: TailHandler[E, R, A, B]): B ! R =
    Free.delay(Free.answer(p, ())((_: Unit, a: A) => h.ret(a), new Free.Answer[E, Unit] {
      def apply[X](cell: okay.effects.Cell[Unit], op: E[X]): X = h.answer[X](op)
    }))
  override def handleLast[E[_], S, R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s0: S,
      h: StatefulTailHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.answer(p, s0)((s: S, a: A) => h.ret(s, a), new Free.Answer[E, S] {
      def apply[X](cell: okay.effects.Cell[S], op: E[X]): X = h.answer[X](cell, op)
    }))
  override def handleLast[E[_], S, R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s0: S,
      h: StatefulHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.handle(p, s0)((s: S, a: A) => h.ret(s, a), new Free.StateClause[E, S, B ! R] {
      def apply[X](s: S, op: E[X], k: (X, S) => B ! R): B ! R = h.op[X](s, op, k)
    }))
  def run[A](p: A ! Pure): A = Free.run(p)
}
