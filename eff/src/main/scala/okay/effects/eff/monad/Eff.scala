package okay.effects.eff.monad

import okay.effects.{Cell, Clauses, Effects, StatefulClauses, StatefulTailClauses, TailClauses, Void}
import okay.effects.Places
import okay.effects.Places.{+, Place, Renaming, toEnd}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

/** THE `Eff` MONAD: a program is data — a value, an operation of `F`, a bind — and its handlers interpret it */
enum Eff[+F[_], A]:
  case Return(a: A)
  case Perform(op: F[A])
  case Bind[F[_], X, A](m: Eff[F, X], f: X => Eff[F, A]) extends Eff[F, A]
  def flatMap[G[x] >: F[x], B](f: A => Eff[G, B]): Eff[G, B] = Bind(this, f)
  def map[B](f: A => B) = flatMap(a => Return(f(a)))

/** an operation of the row `R`: the operation of an effect `E`, and its place in `R` — the row is phantom, of no
 * operation of its own, so an operation says where in it it is */
enum At[R[_], X]:
  case Placed[R[_], E[_], X](at: Place[E, R, ?], op: E[X]) extends At[R, X]

/**
 * THE INTERFACE ON THE `Eff` MONAD. A program over the row `R` is an `Eff` of `R`'s operations, `At[R, *]`; a handler
 * of `E`, the last of `R + E`, is an interpreter of the program, from `R + E` to `R`: a value to its value clause, an
 * operation at `E`'s place to its clause, an operation further in passed on, one place nearer — so the place, typed,
 * says by Scala 3's GADT inference that an operation is the handler's: no cast. One interpreter for every shape of
 * handler (`Level`), its state passed on; `@tailrec` over a bind of a value and over binds nested to the left
 * (re-associated), so a run is in constant stack. A `k` is the rest of the program under the same handler, a
 * function: resumed any number of times.
 */
object OnEff:
  type P[R[_], A] = Eff[[X] =>> At[R, X], A]

  /** the shape of the handler installed, its state of type `S` (`Unit` for none) */
  enum Level[E[_], R[_], A, B, S]:
    case General[E[_], R[_], A, B](h: Clauses[E, A, P[R, B]]) extends Level[E, R, A, B, Unit]
    case Tail[E[_], R[_], A, B](h: TailClauses[E, A, P[R, B]]) extends Level[E, R, A, B, Unit]
    case StateK[E[_], R[_], A, B, S](h: StatefulClauses[E, S, A, P[R, B]]) extends Level[E, R, A, B, S]
    case State[E[_], R[_], A, B, S](h: StatefulTailClauses[E, S, A, P[R, B]]) extends Level[E, R, A, B, S]

  private inline def ret[E[_], R[_], A, B, S](lv: Level[E, R, A, B, S], s: S, a: A): P[R, B] = lv match
    case g: Level.General[e, r, a2, b] => g.h.ret(a)
    case t: Level.Tail[e, r, a2, b] => t.h.ret(a)
    case k: Level.StateK[e, r, a2, b, s2] => k.h.ret(s, a)
    case c: Level.State[e, r, a2, b, s2] => c.h.ret(s, a)

  private final class VarCell[S](var s: S) extends Cell[S]:
    def get: S = s
    def set(t: S): Unit = s = t

  /** `p`, of `R + E`, interpreted by the handler `lv` of `E`, from the state `s`: the loop, `interpret`, and this,
   * what a `k` calls — a call later, not a recursion */
  def handled[E[_], R[_], A, B, S](p: P[R + E, A], lv: Level[E, R, A, B, S], s: S): P[R, B] = interpret(p, lv, s)

  @tailrec private def interpret[E[_], R[_], A, B, S](p: P[R + E, A], lv: Level[E, R, A, B, S], s: S): P[R, B] = p match
    case Eff.Return(a) => ret(lv, s, a)
    case Eff.Perform(o) => interpret(Eff.Bind(Eff.Perform(o), (a: A) => Eff.Return(a)), lv, s)
    case b: Eff.Bind[f, x, a] => b.m match
      case Eff.Return(x) => interpret(b.f(x), lv, s)
      case m: Eff.Bind[f2, y, x2] => interpret(Eff.Bind(m.m, (y: y) => Eff.Bind(m.f(y), b.f)), lv, s)
      case Eff.Perform(o) => o match
        case pl: At.Placed[r, e, x3] => (pl.at: @unchecked) match
          case i: Place.Init[e2, r2, g, rest] =>
            // further in: passed on, one place nearer, the rest under this handler
            Eff.Bind(Eff.Perform(At.Placed(i.s, pl.op)), (x: x) => handled(b.f(x), lv, s))
          case _: Place.Last[r2, e2] => lv match
            case g: Level.General[e3, r3, a3, b3] => g.h.op[x](pl.op, (x: x) => handled(b.f(x), lv, s))
            case t: Level.Tail[e3, r3, a3, b3] => interpret(b.f(t.h.answer[x](pl.op)), lv, s)
            case k: Level.StateK[e3, r3, a3, b3, s3] =>
              k.h.op[x](s, pl.op, (x: x, s1: S) => handled(b.f(x), lv, s1))
            case c: Level.State[e3, r3, a3, b3, s3] =>
              val cell = VarCell[S](s)
              val x = c.h.answer[x](cell, pl.op)
              interpret(b.f(x), lv, cell.get)

  /** `p`'s places renamed from `R1`'s to `R2`'s */
  def remapped[R1[_], R2[_], A](p: P[R1, A], rename: Renaming[R1, R2]): P[R2, A] = renamed(p, rename)

  @tailrec private def renamed[R1[_], R2[_], A](p: P[R1, A], rename: Renaming[R1, R2]): P[R2, A] = p match
    case Eff.Return(a) => Eff.Return(a)
    case Eff.Perform(o) => renamed(Eff.Bind(Eff.Perform(o), (a: A) => Eff.Return(a)), rename)
    case b: Eff.Bind[f, x, a] => b.m match
      case Eff.Return(x) => renamed(b.f(x), rename)
      case m: Eff.Bind[f2, y, x2] => renamed(Eff.Bind(m.m, (y: y) => Eff.Bind(m.f(y), b.f)), rename)
      case Eff.Perform(o) => o match
        case pl: At.Placed[r, e, x3] =>
          Eff.Bind(Eff.Perform(At.Placed(rename(pl.at), pl.op)), (x: x) => remapped(b.f(x), rename))

  /** a program with no effect left: its value */
  @tailrec def value[A](p: P[Void, A]): A = p match
    case Eff.Return(a) => a
    case b: Eff.Bind[f, x, a] => b.m match
      case Eff.Return(x) => value(b.f(x))
      case m: Eff.Bind[f2, y, x2] => value(Eff.Bind(m.m, (y: y) => Eff.Bind(m.f(y), b.f)))
      case Eff.Perform(o) => throw IllegalStateException(s"an operation no handler took: $o")
    case Eff.Perform(o) => throw IllegalStateException(s"an operation no handler took: $o")

/** the interface over the monad: a program an `Eff`, the evidence typed places */
private[eff] class OnEff extends Effects:
  import OnEff.*

  type ![+A, R[_]] = P[R, A @uncheckedVariance]

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

  def pure[A, R[_]](a: A): P[R, A] = Eff.Return(a)
  def flatMap[A, B, R[_]](p: P[R, A])(f: A => P[R, B]): P[R, B] = Eff.Bind(p, f)
  def delay[A, R[_]](p: => P[R, A]): P[R, A] = Eff.Bind(Eff.Return(()), (_: Unit) => p)
  override def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): P[R, X] = Eff.Perform(At.Placed(in, op))

  def split[E[_], R[_], Rest[_], A](p: P[R, A])(implicit s: Split[E, R, Rest]): P[Rest + E, A] = s match
    case _: Place.Last[r, e] => p
    case _ => remapped(p, toEnd(s))
  def embed[R[_], S[_], A](p: P[R, A])(implicit s: Sub[R, S]): P[S, A] = remapped(p, s)

  def handleLast[E[_], R[_], A, B](p: P[R + E, A], h: Handler[E, R, A, B]): P[R, B] =
    delay(handled(p, Level.General(h), ()))
  override def handleLast[E[_], R[_], A, B](p: P[R + E, A], h: TailHandler[E, R, A, B]): P[R, B] =
    delay(handled(p, Level.Tail(h), ()))
  override def handleLast[E[_], S, R[_], A, B](p: P[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B]): P[R, B] =
    delay(handled(p, Level.State(h), s0))
  override def handleLast[E[_], S, R[_], A, B](p: P[R + E, A], s0: S, h: StatefulHandler[E, S, R, A, B]): P[R, B] =
    delay(handled(p, Level.StateK(h), s0))
  def run[A](p: P[Void, A]): A = value(p)
