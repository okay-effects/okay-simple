package okay.simple

/**
 * THE INTERFACE: the types of programs and rows and the primitives on them — no machine, no representation. An
 * implementation is seen through this trait only, so its types are abstract to its users: a program, a row, the
 * evidence that an effect is in a row are whatever the implementation makes them (a place in a list, a key, nothing),
 * and code written here runs on any implementation.
 *
 *  - `A ! R`: a program of value `A` over the row `R`;
 *  - a row: `Pure`, one effect `E`, or `R + E` — `(Reader % String) + (State % Int)`, left to right;
 *  - `Split[E, R, Rest]`: `E` is in `R`, from any place, and `R` without it is `Rest`; `In[E, R]`: `E` is in `R`;
 *    `Sub[R, S]`: every effect of `R` is in `S`;
 *  - `perform`, `split`, `embed`, `handle`, `run`.
 *
 * Scala 2 and Scala 3 each add their conveniences (`Syntax`, in `scala-2/` and `scala-3/`), over these same types:
 * what one compiler's code makes, the other's takes as it is. One application has ONE build of this interface, made
 * by one of the two compilers — Scala 2.13 reads Scala 3's (`-Ytasty-reader`), Scala 3 reads Scala 2.13's — so
 * everything Scala 2 code needs is here, in the source both compile, under the same names; `Syntax` only adds.
 */
trait Effects extends Syntax {
  /** a program of value `A` over the row `R` */
  type ![+A, R]
  /** the row of no effect */
  type Pure
  /** the row `R` and the effect `E` after it */
  type +[R, E]

  /** `E` is in `R`, and `R` without that `E` is `Rest` */
  type Split[E, R, Rest]
  /** `E` is in `R` */
  type In[E, R]
  /** every effect of `R` is in `S` */
  type Sub[R, S]

  /** a context bound, in both compilers: `def greet[R: Has[Reader % String]#L : Has[State % Int]#L]: Int ! R`, the
   * implicit parameters `In[Reader % String, R]`, `In[State % Int, R]` */
  type Has[E] = { type L[R] = In[E, R] }

  implicit def splitOne[E]: Split[E, E, Pure]
  implicit def splitLast[R, E]: Split[E, R + E, R]
  implicit def splitInit[E, R, G, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G]
  implicit def in[E, R, Rest](implicit s: Split[E, R, Rest]): In[E, R]
  implicit def subPure[S]: Sub[Pure, S]
  implicit def subOne[E, S](implicit e: In[E, S]): Sub[E, S]
  implicit def subMore[R, E, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S]

  def pure[A, R](a: A): A ! R
  def flatMap[A, B, R](p: A ! R)(f: A => B ! R): B ! R
  /** the program `p`, built when the run gets to it */
  def delay[A, R](p: => A ! R): A ! R

  /** an operation of `E`: `perform[State % Int].apply[R, Int](State.Get(s => s))` */
  def perform[E](implicit o: Ops[E]): Perform[E, o.F] = new Perform[E, o.F]
  final class Perform[E, F[_]] {
    def apply[R, X](op: F[X])(implicit in: In[E, R]): X ! R = performOp[E, F, R, X](op)
  }
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: In[E, R]): X ! R

  /** the program with `E` taken from wherever it is in its row to the end, where `handle` takes it */
  def split[E, R, Rest, A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E)
  /** the program in a row of more effects */
  def embed[R, S, A](p: A ! R)(implicit s: Sub[R, S]): A ! S
  /** `E`, the last of the row, handled — deeply: its `k` the rest, under this handler, resumable any number of times */
  def handle[E, F[_], R, A, B](p: A ! (R + E))(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): B ! R
  def run[A](p: A ! Pure): A

  /** a handler of the operations `F`: its value's clause and its operations' clause, over the row outside `R` */
  trait Handler[F[_], R, A, B] {
    def ret(a: A): B ! R
    def op[X](op: F[X], k: X => B ! R): B ! R
  }

  /** a handler of an effect with a parameter, `F % S`, its operations `F[S, X]` — with no type lambda to write */
  abstract class HandlerOf[F[_, _], S, R, A, B] extends Handler[({ type L[x] = F[S, x] })#L, R, A, B]

  /** a stateful handler's state where an operation is: read and written by its clause, in place */
  trait Cell[S] { def get: S; def set(s: S): Unit }
  private final class VarCell[S](private var s: S) extends Cell[S] {
    def get: S = s
    def set(t: S): Unit = s = t
  }

  /** a handler whose every clause resumes `k` once, at once, with its answer: an operation ANSWERED IN PLACE — an
   * implementation needs no `k` for it (`Reader`) */
  trait TailHandler[F[_], R, A, B] {
    def ret(a: A): B ! R
    def answer[X](op: F[X]): X
  }
  /** the same with a state of its own, `cell` (`State`, `Writer`): a `k` taken by another handler and resumed twice
   * resumes the state of its time */
  trait StatefulHandler[F[_], S, R, A, B] {
    def ret(s: S, a: A): B ! R
    def answer[X](cell: Cell[S], op: F[X]): X
  }
  abstract class TailHandlerOf[F[_, _], P, R, A, B] extends TailHandler[({ type L[x] = F[P, x] })#L, R, A, B]
  abstract class StatefulHandlerOf[F[_, _], P, S, R, A, B] extends StatefulHandler[({ type L[x] = F[P, x] })#L, S, R, A, B]

  /** `E`, the last, handled by a handler answering in place. By default a `handle`; an implementation may do better */
  def handleTail[E, F[_], R, A, B](p: A ! (R + E))(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): B ! R =
    handle[E, F, R, A, B](p)(new Handler[F, R, A, B] {
      def ret(a: A) = h.ret(a)
      def op[X](op: F[X], k: X => B ! R) = delay(k(h.answer(op)))
    })
  /** `E`, the last, handled by a stateful handler answering in place, from the state `s0`. By default a `handle` */
  def handleStateful[E, F[_], S, R, A, B](p: A ! (R + E))(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): B ! R =
    flatMap(handle[E, F, R, A, S => B ! R](p)(new Handler[F, R, A, S => B ! R] {
      def ret(a: A) = pure((s: S) => h.ret(s, a))
      def op[X](op: F[X], k: X => (S => B ! R) ! R) = pure { (s: S) =>
        val cell = new VarCell[S](s)
        val x = h.answer(cell, op)
        flatMap(k(x))((f: S => B ! R) => f(cell.get))
      }
    }))((f: S => B ! R) => f(s0))

  /** `E` handled from wherever it is in the row: `split`, then `handle` */
  def handleAny[E, F[_], R, Rest, A, B](p: A ! R)(h: Handler[F, Rest, A, B])(implicit s: Split[E, R, Rest],
      o: Ops.Aux[E, F]): B ! Rest = handle[E, F, Rest, A, B](split[E, R, Rest, A](p))(h)

  implicit final class ProgramOps[A, R](p: A ! R) {
    def flatMap[B](f: A => B ! R): B ! R = Effects.this.flatMap(p)(f)
    def map[B](f: A => B): B ! R = Effects.this.flatMap(p)((a: A) => pure[B, R](f(a)))
    def as[B](b: B): B ! R = map(_ => b)
    def andThen[B](q: => B ! R): B ! R = flatMap(_ => q)
    /** this program in a row of more effects */
    def into[S](implicit s: Sub[R, S]): A ! S = embed[R, S, A](p)
  }
}

