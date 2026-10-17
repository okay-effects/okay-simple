package okay.api

/** a stateful handler's state where an operation is: read and written by its clause, in place — of no
 * implementation's, so that one's static code takes it as it is */
trait Cell[S] { def get: S; def set(s: S): Unit }

/**
 * A HANDLER'S CLAUSES: of the operations `O`, the body's value `A`, the programs `P` it makes — `B ! R`, of the row
 * outside. `Handler[E, R, A, B]` is `Clauses[E, A, B ! R]`.
 */
trait Clauses[O[_], A, P] {
  def ret(a: A): P
  /** an operation, and `k`: the rest, under this handler, resumable any number of times */
  def op[X](op: O[X], k: X => P): P
}
/** clauses each resuming `k` once, at once, with an answer: an operation ANSWERED IN PLACE (`Reader`) */
trait TailClauses[O[_], A, P] {
  def ret(a: A): P
  def answer[X](op: O[X]): X
}
/** the same with a state of its own, `cell` (`State`, `Writer`) */
trait StatefulClauses[O[_], S, A, P] {
  def ret(s: S, a: A): P
  def answer[X](cell: Cell[S], op: O[X]): X
}

/** the sum of the row `R` and the effect `E`: a phantom, of no operation of its own. The row `R + E` is the
 * constructor `[X] Sum[R, E, X]` */
final abstract class Sum[R[_], E[_], X]
/** the row of no effect: no operation */
sealed trait Void[X]

/**
 * THE INTERFACE, ONE FOR BOTH COMPILERS. An effect is a type constructor, the GADT of its operations — `Choose`,
 * `State[Int, *]` — and so is a row: `Pure`, one effect `E`, or `R + E`, the effects summed left to right. A program
 * is `A ! R`, of the value `A` over the row `R`. An operation of `E` is an `E[X]`, and a handler of `E` takes it as
 * it is. The evidence — `Split[E, R, Rest]`, `E` in `R` at any place and the rest; `In[E, R]`; `Sub[R, S]` — is
 * abstract: whatever an implementation makes it, found by implicit search.
 *
 * What differs is only how a row is WRITTEN, each compiler's `Effects`: Scala 3 has type lambdas,
 * `Choose + State % Int`; Scala 2 names the effects by proper types and takes their constructor, `#L`:
 * `(*[Choose] + (State % Int))#L`. Here, for both, the row `R + E` is written `({ type L[x] = Sum[R, E, x] })#L`.
 *
 * An implementation is seen through this trait only, so its types are abstract to its users, and code written here
 * runs on any implementation.
 */
trait Effectful {
  /** a program of value `A` over the row `R` */
  type ![+A, R[_]]
  /** the row of no effect */
  type Pure[X] = Void[X]
  /** the operation of `E` answered by an `X`: an `E[X]` */
  type Op[E[_], X] = E[X]

  /** `E` is in `R`, and `R` without that `E` is `Rest` */
  type Split[E[_], R[_], Rest[_]]
  /** `E` is in `R` */
  type In[E[_], R[_]]
  /** every effect of `R` is in `S` */
  type Sub[R[_], S[_]]

  implicit def splitOne[E[_]]: Split[E, E, Pure]
  implicit def splitLast[R[_], E[_]]: Split[E, ({ type L[x] = Sum[R, E, x] })#L, R]
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]):
      Split[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L]
  implicit def in[E[_], R[_], Rest[_]](implicit s: Split[E, R, Rest]): In[E, R]
  implicit def subPure[S[_]]: Sub[Pure, S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S]
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[({ type L[x] = Sum[R, E, x] })#L, S]

  def pure[A, R[_]](a: A): A ! R
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R
  /** the program `p`, built when the run gets to it */
  def delay[A, R[_]](p: => A ! R): A ! R

  /** an operation of `E`: `perform[Choose](Choose.Among(xs))`, the row inferred or given, `.apply[R, X](op)` */
  def perform[E[_]]: Perform[E] = new Perform[E]
  final class Perform[E[_]] {
    def apply[R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = performOp[E, R, X](op)
  }
  protected def performOp[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R

  /** the program with `E` taken from wherever it is in its row to the end, where `handleLast` takes it */
  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! ({ type L[x] = Sum[Rest, E, x] })#L
  /** the program in a row of more effects */
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S
  /** `E`, the last of the row, handled — deeply: its `k` the rest, under this handler, resumable any number of times */
  def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L)(h: Handler[E, R, A, B]): B ! R
  def run[A](p: A ! Pure): A

  /** a handler of `E`: its value's clause, and its operations' clause — each an `E[X]` — over the row outside, `R` */
  type Handler[E[_], R[_], A, B] = Clauses[E, A, B ! R]
  /** a handler whose every clause resumes `k` once, at once, with its answer: an operation ANSWERED IN PLACE — an
   * implementation needs no `k` for it (`Reader`) */
  type TailHandler[E[_], R[_], A, B] = TailClauses[E, A, B ! R]
  /** the same with a state of its own, `cell` (`State`, `Writer`): a `k` taken by another handler and resumed twice
   * resumes the state of its time */
  type StatefulHandler[E[_], S, R[_], A, B] = StatefulClauses[E, S, A, B ! R]
  /** a stateful handler's state where an operation is (`okay.api.Cell`) */
  type Cell[S] = okay.api.Cell[S]

  /** `E`, the last, handled by a handler answering in place. By default a `handleLast` of a general handler giving
   * the answer to `k`; an implementation may do better */
  def handleTailLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L)(h: TailHandler[E, R, A, B]): B ! R =
    handleLast[E, R, A, B](p)(new Clauses[E, A, B ! R] {
      def ret(a: A): B ! R = h.ret(a)
      def op[X](op: E[X], k: X => B ! R): B ! R = delay(k(h.answer(op)))
    })
  /** `E`, the last, handled by a stateful handler answering in place, from the state `s0`. By default a `handleLast`
   * of a general handler of functions of the state */
  def handleStatefulLast[E[_], S, R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L)(s0: S)(
      h: StatefulHandler[E, S, R, A, B]): B ! R =
    flatMap(handleLast[E, R, A, S => B ! R](p)(new Clauses[E, A, (S => B ! R) ! R] {
      def ret(a: A): (S => B ! R) ! R = pure((s: S) => h.ret(s, a))
      def op[X](op: E[X], k: X => (S => B ! R) ! R): (S => B ! R) ! R = pure { (s: S) =>
        val cell = new Effectful.VarCell[S](s)
        val x = h.answer(cell, op)
        flatMap(k(x))((f: S => B ! R) => f(cell.get))
      }
    }))((f: S => B ! R) => f(s0))

  /** `E` handled from wherever it is in the row: by default `split`, then `handleLast`; an implementation may take it
   * from its place at once */
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R)(h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    handleLast[E, Rest, A, B](split[E, R, Rest, A](p))(h)
  /** the same, a handler answering in place */
  def handleTail[E[_], R[_], Rest[_], A, B](p: A ! R)(h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    handleTailLast[E, Rest, A, B](split[E, R, Rest, A](p))(h)
  /** the same, a stateful handler answering in place, from `s0` */
  def handleStateful[E[_], S, R[_], Rest[_], A, B](p: A ! R)(s0: S)(h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    handleStatefulLast[E, S, Rest, A, B](split[E, R, Rest, A](p))(s0)(h)

  implicit final class ProgramOps[A, R[_]](p: A ! R) {
    def flatMap[B](f: A => B ! R): B ! R = Effectful.this.flatMap(p)(f)
    def map[B](f: A => B): B ! R = Effectful.this.flatMap(p)((a: A) => pure[B, R](f(a)))
    def as[B](b: B): B ! R = map(_ => b)
    def andThen[B](q: => B ! R): B ! R = flatMap(_ => q)
    /** this program in a row of more effects */
    def into[S[_]](implicit s: Sub[R, S]): A ! S = embed[R, S, A](p)
  }
}

object Effectful {
  private final class VarCell[S](private var s: S) extends Cell[S] {
    def get: S = s
    def set(t: S): Unit = s = t
  }
}
