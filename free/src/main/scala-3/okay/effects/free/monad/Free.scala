package okay.effects.free.monad

import okay.effects.{Cell, Effects, Void}
import okay.effects.Places
import okay.effects.Places.{+, Place, Renaming, Taken, take, toEnd}
import scala.annotation.tailrec

/** THE `Free` MONAD: a program is data — a value, an operation of `F`, a bind, an operation with its rest, a program
 * built later — and its handlers interpret it */
enum Free[+F[_], +A]:
  case Return[A](a: A) extends Free[Nothing, A]
  case Perform[F[_], A](op: F[A]) extends Free[F, A]
  case Bind[F[_], X, A](m: Free[F, X], f: X => Free[F, A]) extends Free[F, A]
  /** an operation and its rest, one node: `Bind(Perform(op), k)`'s — what a handler passes an operation on as */
  case Inject[F[_], X, A](op: F[X], k: X => Free[F, A]) extends Free[F, A]
  /** the program `p()`, built when the run gets to it */
  case Delay[F[_], A](p: () => Free[F, A]) extends Free[F, A]
  /** bound: an operation with its rest one node at once, `Inject`; anything else a `Bind` */
  inline def flatMap[G[x] >: F[x], B](f: A => Free[G, B]): Free[G, B] = this match
    case Perform(op) => Inject(op, f)
    case _ => Bind(this, f)
  inline def map[B](f: A => B) = flatMap(a => Return(f(a)))

/**
 * THE MONAD'S HANDLERS, over rows of typed places. A program over the row `R` is a `Free` of `R`'s operations,
 * `At[R, *]`; a handler of `E`, the last of `R + E`, interprets it from `R + E` to `R`: a value to its value clause,
 * an operation at `E`'s place to its clause, an operation further in passed on, one place nearer — the place, typed,
 * says by Scala 3's GADT inference that an operation is the handler's: no `Typeable`, no cast.
 *
 * In constant stack: `@tailrec` over a bind of a value and over binds nested to the left (re-associated), and `k` a
 * TRAMPOLINE — it gives back the rest delayed, `Delay(...)`, which the interpreter outside goes on with, not a call
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
      case i: Inject[f2, y, x2] => absurd(i.op)
      case Perform(o) => absurd(o)
    case d: Delay[f, a] => run(d.p())
    case i: Inject[f, y, a] => absurd(i.op)
    case Perform(o) => absurd(o)

  /** an operation of the row of no effect: there is none */
  def absurd[X](o: At[Void, X]): Nothing = o match
    // `@unchecked`: the checker asks for `Absurd` here and calls it unreachable once written — no operation is at
    // the empty effect's place, its operation a `Void[X]`, of which there is none
    case pl: At.Placed[r, e, x] => (pl.at: @unchecked) match
      case _: Place.Only[e2] => pl.op.absurd

  /** the program `p`, built when the run gets to it */
  inline def delay[A, R[_]](p: => A ! R): A ! R = Delay(() => p)

  /** `E`, the last of `R + E`, handled deeply: `ret` the value's clause, `eff` an operation's, its `k` the rest under
   * the same handler, resumable any number of times */
  inline def handle[E[_], R[_], A, B](p: A ! (R + E))(ret: A => B ! R, eff: [X] => (E[X], X => B ! R) => B ! R): B ! R =
    handleAt[E, R + E, R, A, B](p, Place.Last[R, E]())(ret, eff)

  /** `E` handled from its place `in`, anywhere in `R`: each operation met there, `take`n — `E`'s own to the clause,
   * another passed on at its place in `Rest`. No `split`, no program renamed */
  inline def handleAt[E[_], R[_], Rest[_], A, B](p: A ! R, in: Place[E, R, Rest])(ret: A => B ! Rest,
      eff: [X] => (E[X], X => B ! Rest) => B ! Rest): B ! Rest = handling(p, in, ret, eff)

  @tailrec private def handling[E[_], R[_], Rest[_], A, B](p: A ! R, in: Place[E, R, Rest], ret: A => B ! Rest,
      eff: [X] => (E[X], X => B ! Rest) => B ! Rest): B ! Rest = p match
    case Return(a) => ret(a)
    // an operation, the program's last: its rest the value's clause
    case Perform(op) => op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        case _: Taken.Mine[e2, r2] => eff[x3](pl.op, ret)
        case t: Taken.Theirs[e2, e3, r2] => Inject(At.Placed(t.at, pl.op), ret)
    case j: Inject[f, y, a] => j.op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        // the handler's: its clause, `k` a trampoline — the rest built when the run outside gets to it
        case _: Taken.Mine[e2, r2] => eff[x3](pl.op, (x: x3) => Delay(() => handleAt(j.k(x), in)(ret, eff)))
        // passed on, at its place outside: the rest under this handler, given back to the handler outside
        case t: Taken.Theirs[e2, e3, r2] => Inject(At.Placed(t.at, pl.op), (x: x3) => handleAt(j.k(x), in)(ret, eff))
    case d: Delay[f, a] => handling(d.p(), in, ret, eff)
    case b: Bind[f, x, a] => b.m match
      case Return(x) => handling(b.f(x), in, ret, eff)
      case m: Bind[f2, y, x2] => handling(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), in, ret, eff)
      case d: Delay[f2, x2] => handling(Bind(d.p(), b.f), in, ret, eff)
      case j: Inject[f2, y, x2] => handling(Inject(j.op, (y: y) => Bind(j.k(y), b.f)), in, ret, eff)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] => take(in, pl.at) match
          case _: Taken.Mine[e2, r2] => eff[x3](pl.op, (x: x3) => Delay(() => handleAt(b.f(x), in)(ret, eff)))
          case t: Taken.Theirs[e2, e3, r2] => Inject(At.Placed(t.at, pl.op), (x: x3) => handleAt(b.f(x), in)(ret, eff))

  /** the same with a state of its own, from `s`: each clause gets the state of its time, `k` resumed with the answer
   * and the state to go on from — each resumption its own */
  inline def handle[E[_], R[_], A, B, S](p: A ! (R + E), s: S)(ret: (S, A) => B ! R,
      eff: [X] => (S, E[X], (X, S) => B ! R) => B ! R): B ! R = handleAt[E, R + E, R, A, B, S](p, Place.Last[R, E](), s)(ret, eff)

  inline def handleAt[E[_], R[_], Rest[_], A, B, S](p: A ! R, in: Place[E, R, Rest], s: S)(ret: (S, A) => B ! Rest,
      eff: [X] => (S, E[X], (X, S) => B ! Rest) => B ! Rest): B ! Rest = stating(p, in, s, ret, eff)

  @tailrec private def stating[E[_], R[_], Rest[_], A, B, S](p: A ! R, in: Place[E, R, Rest], s: S,
      ret: (S, A) => B ! Rest, eff: [X] => (S, E[X], (X, S) => B ! Rest) => B ! Rest): B ! Rest = p match
    case Return(a) => ret(s, a)
    case Perform(op) => op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        case _: Taken.Mine[e2, r2] => eff[x3](s, pl.op, (x: x3, s1: S) => ret(s1, x))
        case t: Taken.Theirs[e2, e3, r2] => Inject(At.Placed(t.at, pl.op), (x: x3) => ret(s, x))
    case j: Inject[f, y, a] => j.op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        case _: Taken.Mine[e2, r2] =>
          eff[x3](s, pl.op, (x: x3, s1: S) => Delay(() => handleAt(j.k(x), in, s1)(ret, eff)))
        case t: Taken.Theirs[e2, e3, r2] =>
          Inject(At.Placed(t.at, pl.op), (x: x3) => handleAt(j.k(x), in, s)(ret, eff))
    case d: Delay[f, a] => stating(d.p(), in, s, ret, eff)
    case b: Bind[f, x, a] => b.m match
      case Return(x) => stating(b.f(x), in, s, ret, eff)
      case m: Bind[f2, y, x2] => stating(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), in, s, ret, eff)
      case d: Delay[f2, x2] => stating(Bind(d.p(), b.f), in, s, ret, eff)
      case j: Inject[f2, y, x2] => stating(Inject(j.op, (y: y) => Bind(j.k(y), b.f)), in, s, ret, eff)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] => take(in, pl.at) match
          case _: Taken.Mine[e2, r2] =>
            eff[x3](s, pl.op, (x: x3, s1: S) => Delay(() => handleAt(b.f(x), in, s1)(ret, eff)))
          case t: Taken.Theirs[e2, e3, r2] =>
            Inject(At.Placed(t.at, pl.op), (x: x3) => handleAt(b.f(x), in, s)(ret, eff))

  /** `E`, the last of `R + E`, handled by a handler ANSWERING IN PLACE: each operation of `E`'s answered by `ans`,
   * the run going on at once with the answer — no `k`, no trampoline */
  inline def answer[E[_], R[_], A, B](p: A ! (R + E))(ret: A => B ! R, ans: [X] => E[X] => X): B ! R =
    answerAt[E, R + E, R, A, B, Unit](p, Place.Last[R, E](), ())((_, a) => ret(a), [X] => (_: Cell[Unit], op: E[X]) => ans[X](op))

  /** the same with a state of its own, from `s`, in a cell `ans` reads and writes */
  inline def answer[E[_], R[_], A, B, S](p: A ! (R + E), s: S)(ret: (S, A) => B ! R,
      ans: [X] => (Cell[S], E[X]) => X): B ! R = answerAt[E, R + E, R, A, B, S](p, Place.Last[R, E](), s)(ret, ans)

  /** `E` answered in place from its place `in`, anywhere in `R` */
  inline def answerAt[E[_], R[_], Rest[_], A, B](p: A ! R, in: Place[E, R, Rest])(ret: A => B ! Rest,
      ans: [X] => E[X] => X): B ! Rest =
    answerAt[E, R, Rest, A, B, Unit](p, in, ())((_, a) => ret(a), [X] => (_: Cell[Unit], op: E[X]) => ans[X](op))

  /** the same with a state of its own, from `s`, in a cell: one cell a run of the handler, and where an operation is
   * passed on, the rest goes on from a cell of its own, of the state of its time */
  inline def answerAt[E[_], R[_], Rest[_], A, B, S](p: A ! R, in: Place[E, R, Rest], s: S)(ret: (S, A) => B ! Rest,
      ans: [X] => (Cell[S], E[X]) => X): B ! Rest = answering(p, in, Cell.Var(s), ret, ans)

  @tailrec private def answering[E[_], R[_], Rest[_], A, B, S](p: A ! R, in: Place[E, R, Rest], cell: Cell.Var[S],
      ret: (S, A) => B ! Rest, ans: [X] => (Cell[S], E[X]) => X): B ! Rest = p match
    case Return(a) => ret(cell.state, a)
    case Perform(op) => op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        case _: Taken.Mine[e2, r2] => val x = ans[x3](cell, pl.op); ret(cell.state, x)
        case t: Taken.Theirs[e2, e3, r2] =>
          val now = cell.state
          Inject(At.Placed(t.at, pl.op), (x: x3) => ret(now, x))
    case j: Inject[f, y, a] => j.op match
      case pl: At.Placed[r, e, x3] => take(in, pl.at) match
        case _: Taken.Mine[e2, r2] => answering(j.k(ans[x3](cell, pl.op)), in, cell, ret, ans)
        case t: Taken.Theirs[e2, e3, r2] =>
          val now = cell.state
          Inject(At.Placed(t.at, pl.op), (x: x3) => answerAt(j.k(x), in, now)(ret, ans))
    case d: Delay[f, a] => answering(d.p(), in, cell, ret, ans)
    case b: Bind[f, x, a] => b.m match
      case Return(x) => answering(b.f(x), in, cell, ret, ans)
      case m: Bind[f2, y, x2] => answering(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), in, cell, ret, ans)
      case d: Delay[f2, x2] => answering(Bind(d.p(), b.f), in, cell, ret, ans)
      case j: Inject[f2, y, x2] => answering(Inject(j.op, (y: y) => Bind(j.k(y), b.f)), in, cell, ret, ans)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] => take(in, pl.at) match
          case _: Taken.Mine[e2, r2] => answering(b.f(ans[x3](cell, pl.op)), in, cell, ret, ans)
          case t: Taken.Theirs[e2, e3, r2] =>
            val now = cell.state
            Inject(At.Placed(t.at, pl.op), (x: x3) => answerAt(b.f(x), in, now)(ret, ans))

  /** `p`'s places renamed from `R1`'s to `R2`'s */
  inline def remap[R1[_], R2[_], A](p: A ! R1, rename: Renaming[R1, R2]): A ! R2 = remapping(p, rename)

  @tailrec private def remapping[R1[_], R2[_], A](p: A ! R1, rename: Renaming[R1, R2]): A ! R2 = p match
    case Return(a) => Return(a)
    case Perform(op) => op match
      case pl: At.Placed[r, e, x3] => Perform(At.Placed(rename(pl.at), pl.op))
    case j: Inject[f, y, a] => j.op match
      case pl: At.Placed[r, e, x3] => Inject(At.Placed(rename(pl.at), pl.op), (x: x3) => remap(j.k(x), rename))
    case d: Delay[f, a] => Delay(() => remap(d.p(), rename))
    case b: Bind[f, x, a] => b.m match
      case Return(x) => remapping(b.f(x), rename)
      case m: Bind[f2, y, x2] => remapping(Bind(m.m, (y: y) => Bind(m.f(y), b.f)), rename)
      case d: Delay[f2, x2] => remapping(Bind(d.p(), b.f), rename)
      case j: Inject[f2, y, x2] => remapping(Inject(j.op, (y: y) => Bind(j.k(y), b.f)), rename)
      case Perform(op) => op match
        case pl: At.Placed[r, e, x3] => Inject(At.Placed(rename(pl.at), pl.op), (x: x3) => remap(b.f(x), rename))

/** an operation of the row `R`: the operation of an effect `E`, and its place in `R` — the row is phantom, of no
 * operation of its own, so an operation says where in it it is */
enum At[R[_], X]:
  case Placed[R[_], E[_], X](at: Place[E, R, ?], op: E[X]) extends At[R, X]

/** THE INTERFACE ON THE `Free` MONAD: a program a `Free`, the evidence typed places, every handler `Free.handle` or
 * `Free.handle`, or one answering in place `Free.answer` */
class OnFree extends Effects:
  import Free.{!, Bind, Perform, Return}

  type ![+A, R[_]] = Free.![A, R]

  type Split[E[_], R[_], Rest[_]] = Place[E, R, Rest]
  type In[E[_], R[_]] = Place[E, R, ?]
  type Sub[R[_], S[_]] = Renaming[R, S]

  implicit inline def splitOne[E[_]]: Split[E, E, Pure] = Place.Only[E]()
  implicit inline def splitLast[R[_], E[_]]: Split[E, R + E, R] = Place.Last[R, E]()
  implicit inline def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Place.Init[E, R, G, Rest](s)
  implicit inline def subPure[S[_]]: Sub[Pure, S] = Places.subPure[S]
  implicit inline def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Places.subOne(e)
  implicit inline def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Places.subMore(r, e)

  inline def pure[A, R[_]](a: A): A ! R = Return(a)
  inline def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = p.flatMap(f)
  inline def delay[A, R[_]](p: => A ! R): A ! R = Free.delay(p)
  override inline def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = Perform(At.Placed(in, op))

  inline def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E) = s match
    case _: Place.Last[r, e] => p
    case _ => Free.delay(Free.remap(p, toEnd(s)))
  inline def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = Free.delay(Free.remap(p, s))

  // a handler takes its effect from its place at once: no `split`, no program renamed
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Free.delay(Free.handleAt(p, s)(h.ret, [X] => (op: E[X], k: X => B ! Rest) => h.op[X](op, k)))
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Free.delay(Free.answerAt(p, s)(h.ret, [X] => (op: E[X]) => h.answer[X](op)))
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Free.delay(Free.handleAt(p, s)(h.ret, [X] => (op: E[X], k: X => B ! Rest) => h.abort[X](op)))
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Free.delay(Free.answerAt(p, s, s0)(h.ret, [X] => (cell: Cell[S], op: E[X]) => h.answer[X](cell, op)))
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    Free.delay(Free.handleAt(p, s, s0)(h.ret, [X] => (st: S, op: E[X], k: (X, S) => B ! Rest) => h.op[X](st, op, k)))

  inline def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: Handler[E, R, A, B]): B ! R =
    Free.delay(Free.handle(p)(h.ret, [X] => (op: E[X], k: X => B ! R) => h.op[X](op, k)))
  override inline def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: TailHandler[E, R, A, B]): B ! R =
    Free.delay(Free.answer(p)(h.ret, [X] => (op: E[X]) => h.answer[X](op)))
  override inline def handleLast[E[_], S, R[_], A, B](p: A ! (R + E), s0: S, h: StatefulTailHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.answer(p, s0)(h.ret, [X] => (cell: Cell[S], op: E[X]) => h.answer[X](cell, op)))
  override inline def handleLast[E[_], S, R[_], A, B](p: A ! (R + E), s0: S, h: StatefulHandler[E, S, R, A, B]): B ! R =
    Free.delay(Free.handle(p, s0)(h.ret, [X] => (s: S, op: E[X], k: (X, S) => B ! R) => h.op[X](s, op, k)))
  inline def run[A](p: A ! Pure): A = Free.run(p)
