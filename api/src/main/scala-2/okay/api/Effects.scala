package okay.api

/**
 * AN EFFECT'S NAME, a proper type, of its operations `L`: Scala 2 has no type alias whose result is a type
 * constructor, so a row is written by names — `*[Choose] + (State % Int)` — and taken as the constructor the names
 * make, `#L`.
 */
trait Effect { type L[X] }

/** the effect of the operations `F`, of no parameter: `*[Choose]#L` is `Choose` */
final abstract class *[F[_]] extends Effect { type L[X] = F[X] }

/** the effect `F` with its parameter `S`: `(State % Int)#L`, its operations `State[Int, X]` */
final abstract class %[F[_, _], S] extends Effect { type L[X] = F[S, X] }

/** the row `R` and the effect `E` after it, by their names: `(*[Choose] + (State % Int))#L` */
final abstract class +[R <: Effect, E <: Effect] extends Effect { type L[X] = Sum[R#L, E#L, X] }

/**
 * THE INTERFACE AS SCALA 2 WRITES IT: `Effectful`, and its rows by names. One precedence for all infix types: each
 * `%` in parentheses.
 *
 * {{{
 * type Row[X] = ((Reader % String) + (State % Int))#L[X]
 * def greet[R[_]](implicit r: In[(Reader % String)#L, R], s: In[(State % Int)#L, R]): Int ! R
 * }}}
 */
trait Effects extends Effectful {
  /** the names here too: an implementation imported gives them (`import Simple._`) */
  type %[F[_, _], S] = okay.api.%[F, S]
  type *[F[_]] = okay.api.*[F]
  type +[R <: Effect, E <: Effect] = okay.api.+[R, E]
  /** a context bound: `def greet[R[_]: Has[(Reader % String)#L]#L]: Int ! R` */
  type Has[E[_]] = { type L[R[_]] = In[E, R] }
}
