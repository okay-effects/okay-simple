package okay.api

/**
 * AN EFFECT'S NAME, of its operations. An effect's operations are a GADT, `F[X]` an operation answered by an `X`; in
 * a row an effect is named by a proper type — Scala 2 has no type alias whose result is a type constructor — and the
 * name says its operations: `type Ops[X]`. So the type of an effect's operations is computed from its name, the same
 * wherever it is asked, and an operation and the handler it goes to are of one type (`Effects.Op`).
 */
trait Effect { type Ops[X] }

/** a name of the operations `F` */
trait Name[F[_]] extends Effect { type Ops[X] = F[X] }

/** the effect of the operations `F`, of no parameter, a name: `*[Choose]`, its operations `Choose[X]` */
final abstract class *[F[_]] extends Name[F]

/** the effect `F` with its parameter `S`, a name: `State % Int`, its operations `State[Int, X]` */
final abstract class %[F[_, _], S] extends Name[({ type L[x] = F[S, x] })#L]

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
 *  - `perform`, `handle` (an effect from any place), `run`; under them `split`, `handleLast` (the last effect), `embed`.
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
  type Split[E <: Effect, R, Rest]
  /** `E` is in `R` */
  type In[E <: Effect, R]
  /** every effect of `R` is in `S` */
  type Sub[R, S]

  /** a context bound, in both compilers: `def greet[R: Has[Reader % String]#L : Has[State % Int]#L]: Int ! R`, the
   * implicit parameters `In[Reader % String, R]`, `In[State % Int, R]` */
  type Has[E <: Effect] = { type L[R] = In[E, R] }

  implicit def splitOne[E <: Effect]: Split[E, E, Pure]
  implicit def splitLast[R, E <: Effect]: Split[E, R + E, R]
  implicit def splitInit[E <: Effect, R, G, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G]
  implicit def in[E <: Effect, R, Rest](implicit s: Split[E, R, Rest]): In[E, R]
  implicit def subPure[S]: Sub[Pure, S]
  implicit def subOne[E <: Effect, S](implicit e: In[E, S]): Sub[E, S]
  implicit def subMore[R, E <: Effect, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S]

  def pure[A, R](a: A): A ! R
  def flatMap[A, B, R](p: A ! R)(f: A => B ! R): B ! R
  /** the program `p`, built when the run gets to it */
  def delay[A, R](p: => A ! R): A ! R

  /** an operation of `E`: `perform[State % Int].apply[R, Int](State.Get(s => s))` */
  def perform[E <: Effect]: Perform[E] = new Perform[E]
  final class Perform[E <: Effect] {
    def apply[R, X](op: Op[E, X])(implicit in: In[E, R]): X ! R = performOp[E, R, X](op)
  }
  protected def performOp[E <: Effect, R, X](op: Op[E, X])(implicit in: In[E, R]): X ! R

  /** the program with `E` taken from wherever it is in its row to the end, where `handleLast` takes it */
  def split[E <: Effect, R, Rest, A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E)
  /** the program in a row of more effects */
  def embed[R, S, A](p: A ! R)(implicit s: Sub[R, S]): A ! S
  /** `E`, the last of the row, handled — deeply: its `k` the rest, under this handler, resumable any number of times */
  def handleLast[E <: Effect, R, A, B](p: A ! (R + E))(h: Handler[E, R, A, B]): B ! R
  def run[A](p: A ! Pure): A

  /** a handler of the effect `E`: its value's clause, and its operations' clause — each operation of `E`'s,
   * `Op[E, X]`, the type the name says — over the row outside, `R` */
  trait Handler[E <: Effect, R, A, B] {
    def ret(a: A): B ! R
    def op[X](op: Op[E, X], k: X => B ! R): B ! R
  }

  /** a stateful handler's state where an operation is: read and written by its clause, in place */
  trait Cell[S] { def get: S; def set(s: S): Unit }
  private final class VarCell[S](private var s: S) extends Cell[S] {
    def get: S = s
    def set(t: S): Unit = s = t
  }

  /** a handler whose every clause resumes `k` once, at once, with its answer: an operation ANSWERED IN PLACE — an
   * implementation needs no `k` for it (`Reader`) */
  trait TailHandler[E <: Effect, R, A, B] {
    def ret(a: A): B ! R
    def answer[X](op: Op[E, X]): X
  }
  /** the same with a state of its own, `cell` (`State`, `Writer`): a `k` taken by another handler and resumed twice
   * resumes the state of its time */
  trait StatefulHandler[E <: Effect, S, R, A, B] {
    def ret(s: S, a: A): B ! R
    def answer[X](cell: Cell[S], op: Op[E, X]): X
  }

  /** `E`, the last, handled by a handler answering in place. By default a `handleLast`; an implementation may do better */
  def handleTailLast[E <: Effect, R, A, B](p: A ! (R + E))(h: TailHandler[E, R, A, B]): B ! R =
    handleLast[E, R, A, B](p)(new Handler[E, R, A, B] {
      def ret(a: A) = h.ret(a)
      def op[X](op: Op[E, X], k: X => B ! R) = delay(k(h.answer(op)))
    })
  /** `E`, the last, handled by a stateful handler answering in place, from the state `s0`. By default a `handleLast` */
  def handleStatefulLast[E <: Effect, S, R, A, B](p: A ! (R + E))(s0: S)(h: StatefulHandler[E, S, R, A, B]): B ! R =
    flatMap(handleLast[E, R, A, S => B ! R](p)(new Handler[E, R, A, S => B ! R] {
      def ret(a: A) = pure((s: S) => h.ret(s, a))
      def op[X](op: Op[E, X], k: X => (S => B ! R) ! R) = pure { (s: S) =>
        val cell = new VarCell[S](s)
        val x = h.answer(cell, op)
        flatMap(k(x))((f: S => B ! R) => f(cell.get))
      }
    }))((f: S => B ! R) => f(s0))

  /** `E` handled from wherever it is in the row: by default `split`, then `handleLast`; an implementation may take it
   * from its place at once */
  def handle[E <: Effect, R, Rest, A, B](p: A ! R)(h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    handleLast[E, Rest, A, B](split[E, R, Rest, A](p))(h)
  /** the same, a handler answering in place */
  def handleTail[E <: Effect, R, Rest, A, B](p: A ! R)(h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    handleTailLast[E, Rest, A, B](split[E, R, Rest, A](p))(h)
  /** the same, a stateful handler answering in place, from `s0` */
  def handleStateful[E <: Effect, S, R, Rest, A, B](p: A ! R)(s0: S)(h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest =
    handleStatefulLast[E, S, Rest, A, B](split[E, R, Rest, A](p))(s0)(h)

  implicit final class ProgramOps[A, R](p: A ! R) {
    def flatMap[B](f: A => B ! R): B ! R = Effects.this.flatMap(p)(f)
    def map[B](f: A => B): B ! R = Effects.this.flatMap(p)((a: A) => pure[B, R](f(a)))
    def as[B](b: B): B ! R = map(_ => b)
    def andThen[B](q: => B ! R): B ! R = flatMap(_ => q)
    /** this program in a row of more effects */
    def into[S](implicit s: Sub[R, S]): A ! S = embed[R, S, A](p)
  }
}

