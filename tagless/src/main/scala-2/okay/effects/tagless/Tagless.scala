package okay.effects.tagless

/**
 * TAGLESS, AS SCALA 2 WRITES IT: `import okay.effects.tagless.Tagless._`, `import Tagless.std._` — programs over no
 * implementation, its rows by names (`#L`); run with an `Effects` in the implicit context:
 * `implicit val fx: Effects = Machine; run(p)`.
 */
object Tagless extends Programs {
  type %[F[_, _], S] = okay.effects.%[F, S]
  type *[F[_]] = okay.effects.*[F]
  type +[R <: okay.effects.Effect, E <: okay.effects.Effect] = okay.effects.+[R, E]
  /** a context bound: `def greet[R[_]: Has[(Reader % String)#L]#L]: Int ! R` */
  type Has[E[_]] = { type L[R[_]] = In[E, R] }

  implicit final class ProgramOps[A, R[_]](private val p: A ! R) extends AnyVal {
    def flatMap[B](f: A => B ! R): B ! R = Tagless.flatMap(p)(f)
    def map[B](f: A => B): B ! R = Tagless.map(p)(f)
    def as[B](b: B): B ! R = map(_ => b)
    def andThen[B](q: => B ! R): B ! R = flatMap(_ => q)
    def into[S[_]](implicit s: Sub[R, S]): A ! S = embed[R, S, A](p)
  }
}
