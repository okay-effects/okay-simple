package okay.effects.simple.free.monad

import okay.effects.{Cell, Effects, Void}
import okay.effects.Places
import okay.effects.Places.{+, Place, Renaming, toEnd}
import scala.annotation.tailrec

/** THE `Free` MONAD: a program is data — a value, an operation of `F`, a bind, a program built later — and its
 * handlers interpret it */
enum Free[+F[_], +A]:
  case Return[A](a: A) extends Free[Nothing, A]
  case Perform[F[_], A](op: F[A]) extends Free[F, A]
  case Bind[F[_], X, A](m: Free[F, X], f: X => Free[F, A]) extends Free[F, A]
  /** the program `p()`, built when the run gets to it */
  case Delay[F[_], A](p: () => Free[F, A]) extends Free[F, A]
  def flatMap[G[x] >: F[x], B](f: A => Free[G, B]): Free[G, B] = Bind(this, f)
  def map[B](f: A => B) = flatMap(a => Return(f(a)))

/**
 * THE MONAD'S HANDLERS, over rows of typed places. A program over the row `R` is a `Free` of `R`'s operations,
 * `At[R, *]`; a handler of `E`, the last of `R + E`, interprets it from `R + E` to `R`: a value to its value clause,
 * an operation at `E`'s place to its clause, an operation further in passed on, one place nearer — the place, typed,
 * says by Scala 3's GADT inference that an operation is the handler's: no `Typeable`, no cast.
 *
 * In constant stack: `@tailrec` over a bind of a value and over binds nested to the left (re-associated), and `k` a
 * TRAMPOLINE — it gives back a bind, `Bind(Return(x), ...)`, which the interpreter outside goes on with, not a call
 * into the rest: a clause resuming `k` at once does not nest a call per operation.
 */
object Free:
  /** a program of the value `A` over the row `R`: a `Free` of `R`'s operations */
  infix type ![+A, R[_]] = Free[[X] =>> At[R, X], A]
  /** the operations of the row of no effect: none — no place in `Void` */
  type Pure[X] = At[Void, X]

  /** a program of no effect: its value. An operation of it there is none: its only place `Only[Void]`, its operation
   * a `Void[X]`, of which there is no value — absurd */
  @tailrec def run[A](p: Free[Pure, A]): A = p match
    case Return(a) => a
    case b: Bind[f, x, a] => b.m match
      case Return(x) => run(b.f(x))
      case m: Bind[f2, y, x2] => run(Bind(m.m, (y: y) => Bind(m.f(y), b.f)))
      case d: Delay[f2, x2] => run(Bind(d.p(), b.f))
      case Perform(o) => absurd(o)
    case d: Delay[f, a] => run(d.p())
    case Perform(o) => absurd(o)

  /** an operation of the row of no effect: there is none */
  def absurd[X](o: At[Void, X]): Nothing = o match
    case pl: At.Placed[r, e, x] => pl.at match
      case _: Place.Only[e2] => pl.op.absurd

  /** the program `p`, built when the run gets to it */
  def delay[A, R[_]](p: => A ! R): A ! R = Delay(() => p)

  /** `E`, the last of `R + E`, handled deeply: `ret` the value's clause, `eff` an operation's, its `k` the rest under
   * the same handler, resumable any number of times */
  def handle[E[_], R[_], A, B](p: A ! (R + E))(ret: A => B ! R, eff: [X] => (E[X], X => B ! R) => B ! R): B ! R =
    handle[E, R, A, B, Unit](p, ())((_, a) => ret(a), [X] => (_: Unit, op: E[X], k: (X, Unit) => B ! R) =>
      eff[X](op, (x: X) => k(x, ())))

  /** the same with a state of its own, from `s`: each clause gets the state of its time, `k` resumed with the answer
   * and the state to go on from — each resumption its own */
  def handle[E[_], R[_], A, B, S](p: A ! (R + E), s: S)(ret: (S, A) => B ! R,
      eff: [X] => (S, E[X], (X, S) => B ! R) => B ! R): B ! R = handling(p, s, ret, eff)

  @tailrec private def handling[E[_], R[_], A, B, S](p: A ! (R + E), s: S, ret: (S, A) => B ! R,
      eff: [X] => (S, E[X], (X, S) => B ! R) => B ! R): B ! R = p match
    case Return(a) => ret(s, a)
    case Perform(op) => on(op, s, eff, (a: A, s1: S) => ret(s1, a))
    case d: Delay[f, a] => handling(d.p(), s, ret, eff)
    case b: Bind[f, x, a] => b.m match
      case Return(x) => handling(b.f(x), s, ret, eff)
      case Perform(op) =>
        on(op, s, eff, (x: x, s1: S) => Bind(Return(x), (x: x) => handle(b.f(x), s1)(ret, eff)))
      case m: Bind[f2, y, x2] => handling(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), s, ret, eff)
      case d: Delay[f2, x2] => handling(Bind(d.p(), b.f), s, ret, eff)

  /** an operation at `R + E`: `E`'s to the handler's clause, any other passed on, one place nearer, then `k` */
  private def on[E[_], R[_], X, B, S](op: At[R + E, X], s: S, eff: [Y] => (S, E[Y], (Y, S) => B ! R) => B ! R,
      k: (X, S) => B ! R): B ! R = op match
    case pl: At.Placed[r, e, x] => (pl.at: @unchecked) match
      case _: Place.Last[r2, e2] => eff[X](s, pl.op, k)
      case i: Place.Init[e2, r2, g, rest] => Bind(Perform(At.Placed(i.s, pl.op)), (x: X) => k(x, s))

  /** `E`, the last of `R + E`, handled by a handler ANSWERING IN PLACE: each operation of `E`'s answered by `ans`,
   * the run going on at once with the answer — no `k`, no trampoline */
  def answer[E[_], R[_], A, B](p: A ! (R + E))(ret: A => B ! R, ans: [X] => E[X] => X): B ! R =
    answer[E, R, A, B, Unit](p, ())((_, a) => ret(a), [X] => (_: Cell[Unit], op: E[X]) => ans[X](op))

  /** the same with a state of its own, from `s`, in a cell `ans` reads and writes: one cell a run of the handler, and
   * where an operation is passed on, the rest goes on from a cell of its own, of the state of its time */
  def answer[E[_], R[_], A, B, S](p: A ! (R + E), s: S)(ret: (S, A) => B ! R,
      ans: [X] => (Cell[S], E[X]) => X): B ! R = answering(p, Cell.Var(s), ret, ans)

  @tailrec private def answering[E[_], R[_], A, B, S](p: A ! (R + E), cell: Cell.Var[S], ret: (S, A) => B ! R,
      ans: [X] => (Cell[S], E[X]) => X): B ! R = p match
    case Return(a) => ret(cell.state, a)
    case Perform(op) => answering(Bind(Perform(op), (a: A) => Return(a)), cell, ret, ans)
    case d: Delay[f, a] => answering(d.p(), cell, ret, ans)
    case b: Bind[f, x, a] => b.m match
      case Return(x) => answering(b.f(x), cell, ret, ans)
      case m: Bind[f2, y, x2] => answering(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), cell, ret, ans)
      case d: Delay[f2, x2] => answering(Bind(d.p(), b.f), cell, ret, ans)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] => (pl.at: @unchecked) match
          case _: Place.Last[r2, e2] => answering(b.f(ans[x3](cell, pl.op)), cell, ret, ans)
          case i: Place.Init[e2, r2, g, rest] =>
            val now = cell.state
            Bind(Perform(At.Placed(i.s, pl.op)), (x: x) => answer(b.f(x), now)(ret, ans))

  /** `p`'s places renamed from `R1`'s to `R2`'s */
  def remap[R1[_], R2[_], A](p: A ! R1, rename: Renaming[R1, R2]): A ! R2 = remapping(p, rename)

  @tailrec private def remapping[R1[_], R2[_], A](p: A ! R1, rename: Renaming[R1, R2]): A ! R2 = p match
    case Return(a) => Return(a)
    case Perform(op) => remapping(Bind(Perform(op), (a: A) => Return(a)), rename)
    case d: Delay[f, a] => Delay(() => remap(d.p(), rename))
    case b: Bind[f, x, a] => b.m match
      case Return(x) => remapping(b.f(x), rename)
      case m: Bind[f2, y, x2] => remapping(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), rename)
      case d: Delay[f2, x2] => remapping(Bind(d.p(), b.f), rename)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] =>
          Bind(Perform(At.Placed(rename(pl.at), pl.op)), (x: x) => remap(b.f(x), rename))

/** an operation of the row `R`: the operation of an effect `E`, and its place in `R` — the row is phantom, of no
 * operation of its own, so an operation says where in it it is */
enum At[R[_], X]:
  case Placed[R[_], E[_], X](at: Place[E, R, ?], op: E[X]) extends At[R, X]

/** THE INTERFACE ON THE `Free` MONAD: a program a `Free`, the evidence typed places, every handler `Free.handle` or
 * `Free.handle`, or one answering in place `Free.answer` or `Free.answer` */
private[free] class OnFree extends Effects:
  import Free.{!, Bind, Perform, Return}

  type ![+A, R[_]] = Free.![A, R]

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

  def pure[A, R[_]](a: A): A ! R = Return(a)
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Bind(p, f)
  def delay[A, R[_]](p: => A ! R): A ! R = Free.delay(p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = Perform(At.Placed(in, op))

  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E) = s match
    case _: Place.Last[r, e] => p
    case _ => Free.delay(Free.remap(p, toEnd(s)))
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = Free.delay(Free.remap(p, s))

  def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: Handler[E, R, A, B]): B ! R =
    Free.delay(Free.handle(p)(h.ret, [X] => (op: E[X], k: X => B ! R) => h.op[X](op, k)))
  override def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: TailHandler[E, R, A, B]): B ! R =
    Free.delay(Free.answer(p)(h.ret, [X] => (op: E[X]) => h.answer[X](op)))
  override def handleLast[E[_], S, R[_], A, B](p: A ! (R + E), s0: S, h: StatefulTailHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.answer(p, s0)(h.ret, [X] => (cell: Cell[S], op: E[X]) => h.answer[X](cell, op)))
  override def handleLast[E[_], S, R[_], A, B](p: A ! (R + E), s0: S, h: StatefulHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.handle(p, s0)(h.ret, [X] => (s: S, op: E[X], k: (X, S) => B ! R) => h.op[X](s, op, k)))
  def run[A](p: A ! Pure): A = Free.run(p)
