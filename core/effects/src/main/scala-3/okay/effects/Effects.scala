package okay.effects

import scala.annotation.targetName

/** the effect `F` with its parameter `S`: `State % Int`, the operations `State[Int, X]` */
infix type %[F[_, _], S] = [X] =>> F[S, X]
/** the effect `F` itself: `*[Choose]` is `Choose`, kept for code written alike for both compilers */
type *[F[_]] = [X] =>> F[X]

/**
 * THE INTERFACE AS SCALA 3 WRITES IT: `Effectful`, and its rows by type lambdas — `+`, `%` — so a row is written as
 * it is meant, `Choose + Reader % String + State % Int`, an effect of no parameter by its own name, and its
 * requirements as context bounds, `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R`.
 */
trait Effects extends Effectful:
  /** the row `R` and the effect `E` after it */
  type +[R[_], E[_]] = [X] =>> Sum[R, E, X]
  /** the names here too: an implementation imported gives them (`import Machine.*`) */
  type %[F[_, _], S] = [X] =>> F[S, X]
  type *[F[_]] = [X] =>> F[X]
  /** THE ROW OF A NAME, applied — the one spelling both compilers read: `type F[X] = Row[*[Pure] + *[Face], X]`,
   * `Row[(Reader % String) + (State % Int), X]`. Here the name is the row already, applied; in Scala 2 its `L` at `X`
 */
  type Row[N[_], X] = N[X]
  /** a context bound: `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R` */
  type Has[E[_]] = [R[_]] =>> In[E, R]

  /** the standard effects on this implementation: `import Machine.std.*`. Final, a stable path to import from; made
   * by `newStd`, which an implementation may override with a `Std` of better handlers */
  final val std: Std[this.type] = newStd
  protected def newStd: Std[this.type] = new Std[this.type](this)
  /** delimited control changing answer types (`Delimited`), made by `newDelimited`: by default continuation-passing
   * over this implementation's programs; an implementation with continuations of its own overrides it */
  final val delimited: Delimited[this.type] = newDelimited
  protected def newDelimited: Delimited[this.type] = new Delimited.ByCps[this.type](this)

  /** a program's methods: `p.flatMap(f)`, `p.map(f)`, `p.into[S]` — extensions, no object made for them */
  extension [A, R[_]](p: A ! R)
    @targetName("programFlatMap") inline def flatMap[B](f: A => B ! R): B ! R = Effects.this.flatMap[A, B, R](p)(f)
    @targetName("programMap") inline def map[B](f: A => B): B ! R = Effects.this.map[A, B, R](p)(f)
    inline def as[B](b: B): B ! R = Effects.this.map[A, B, R](p)(_ => b)
    inline def andThen[B](inline q: B ! R): B ! R = Effects.this.flatMap[A, B, R](p)(_ => q)
    /** this program in a row of more effects */
    inline def into[S[_]](using s: Sub[R, S]): A ! S = embed[R, S, A](p)
