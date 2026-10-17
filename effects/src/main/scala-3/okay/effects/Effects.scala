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
  type %[F[_, _], S] = okay.effects.%[F, S]
  type *[F[_]] = okay.effects.*[F]
  /** a context bound: `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R` */
  type Has[E[_]] = [R[_]] =>> In[E, R]

  /** a program's methods: `p.flatMap(f)`, `p.map(f)`, `p.into[S]` — extensions, no object made for them */
  extension [A, R[_]](p: A ! R)
    @targetName("programFlatMap") def flatMap[B](f: A => B ! R): B ! R = Effects.this.flatMap[A, B, R](p)(f)
    @targetName("programMap") def map[B](f: A => B): B ! R = Effects.this.map[A, B, R](p)(f)
    def as[B](b: B): B ! R = Effects.this.map[A, B, R](p)(_ => b)
    def andThen[B](q: => B ! R): B ! R = Effects.this.flatMap[A, B, R](p)(_ => q)
    /** this program in a row of more effects */
    def into[S[_]](using s: Sub[R, S]): A ! S = embed[R, S, A](p)
