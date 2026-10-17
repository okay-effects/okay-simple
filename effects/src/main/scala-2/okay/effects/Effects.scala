package okay.effects

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
  /** the names here too: an implementation imported gives them (`import Machine._`) */
  type %[F[_, _], S] = okay.effects.%[F, S]
  type *[F[_]] = okay.effects.*[F]
  type +[R <: Effect, E <: Effect] = okay.effects.+[R, E]
  /** a context bound: `def greet[R[_]: Has[(Reader % String)#L]#L]: Int ! R` */
  type Has[E[_]] = { type L[R[_]] = In[E, R] }

  /** a program's methods: `p.flatMap(f)`, `p.map(f)`, `p.into[S]` */
  implicit final class ProgramOps[A, R[_]](p: A ! R) {
    def flatMap[B](f: A => B ! R): B ! R = Effects.this.flatMap(p)(f)
    def map[B](f: A => B): B ! R = Effects.this.map(p)(f)
    def as[B](b: B): B ! R = map(_ => b)
    def andThen[B](q: => B ! R): B ! R = flatMap(_ => q)
    /** this program in a row of more effects */
    def into[S[_]](implicit s: Sub[R, S]): A ! S = embed[R, S, A](p)
  }
}
