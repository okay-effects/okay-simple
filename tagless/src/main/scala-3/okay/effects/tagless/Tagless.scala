package okay.effects.tagless

import scala.annotation.targetName
import okay.effects.Sum

/**
 * TAGLESS, AS SCALA 3 WRITES IT: `import okay.effects.tagless.Tagless.*`, `import Tagless.std.*` — programs over no
 * implementation, its rows by type lambdas; run with an `Effects` given: `given Effects = Machine; run(p)`.
 */
object Tagless extends Programs:
  type +[R[_], E[_]] = [X] =>> Sum[R, E, X]
  type %[F[_, _], S] = okay.effects.%[F, S]
  type *[F[_]] = okay.effects.*[F]
  /** a context bound: `def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R` */
  type Has[E[_]] = [R[_]] =>> In[E, R]

  extension [A, R[_]](p: A ! R)
    @targetName("programFlatMap") def flatMap[B](f: A => B ! R): B ! R = Tagless.flatMap[A, B, R](p)(f)
    @targetName("programMap") def map[B](f: A => B): B ! R = Tagless.map[A, B, R](p)(f)
    def as[B](b: B): B ! R = Tagless.map[A, B, R](p)(_ => b)
    def andThen[B](q: => B ! R): B ! R = Tagless.flatMap[A, B, R](p)(_ => q)
    def into[S[_]](using s: Sub[R, S]): A ! S = embed[R, S, A](p)
