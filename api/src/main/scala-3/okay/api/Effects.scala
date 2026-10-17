package okay.api

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
  /** the names here too: an implementation imported gives them (`import Simple.*`) */
  type %[F[_, _], S] = okay.api.%[F, S]
  type *[F[_]] = okay.api.*[F]
  /** a context bound: `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R` */
  type Has[E[_]] = [R[_]] =>> In[E, R]
