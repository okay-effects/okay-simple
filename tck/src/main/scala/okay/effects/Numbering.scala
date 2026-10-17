package okay.effects


/** AN EFFECT FOR A STATEFUL GENERAL HANDLER: numbers given out, and forks — each branch resumed with its own state */
sealed trait Gen[X]
object Gen {
  /** the next number */
  final case class Next[X](answer: Int => X) extends Gen[X]
  /** two branches: `false` from the state as it is, `true` from 10 further */
  final case class Fork[X](answer: Boolean => X) extends Gen[X]
  /** no more: this branch dropped */
  final case class Stop[X]() extends Gen[X]
}

/** programs and the handler, one source for both compilers: a row of one effect, or of two by aliases */
class Numbering(val fx: Runner) {
  import fx._
  import fx.std._

  def next[R[_]](implicit in: In[Gen, R]): Int ! R = perform[Gen].apply[R, Int](Gen.Next(n => n))
  def fork[R[_]](implicit in: In[Gen, R]): Boolean ! R = perform[Gen].apply[R, Boolean](Gen.Fork(b => b))
  def stop[R[_], X](implicit in: In[Gen, R]): X ! R = perform[Gen].apply[R, X](Gen.Stop())

  /** each branch's value and the state it ended with: `k` resumed with the next state, twice at a fork */
  def numbering[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Gen, R, Rest]): List[(Int, A)] ! Rest =
    handle[Gen, Int, R, Rest, A, List[(Int, A)]](p, 0, new StatefulHandler[Gen, Int, Rest, A, List[(Int, A)]] {
      def ret(n: Int, a: A) = pure(List((n, a)))
      def op[X](n: Int, op: Gen[X], k: (X, Int) => List[(Int, A)] ! Rest) = op match {
        case Gen.Next(answer) => k(answer(n), n + 1)
        case Gen.Fork(answer) => k(answer(false), n).flatMap(l => k(answer(true), n + 10).map(l ++ _))
        case Gen.Stop() => pure(Nil)
      }
    })

  def forked: Int ! Gen =
    for { a <- next[Gen]; b <- fork[Gen]; c <- next[Gen]; _ <- if (b && c > 100) stop[Gen, Unit] else pure[Unit, Gen](()) } yield a * 100 + c
  def numbered = run(numbering(forked))

  /** beside a state, outside it: the state one through both branches, each branch its own number */
  type SI[X] = State[Int, X]
  type R2[X] = Sum[Gen, SI, X]
  def withState: (Boolean, Int, Int) ! R2 =
    for {
      _ <- next[R2]; b <- fork[R2]; n <- get[Int, R2]; _ <- put[Int, R2](n + 1); c <- next[R2]
    } yield (b, n, c)
  def numberedInState = run(state(5)(numbering(withState)))

  /** a branch stopped: dropped, its state with it */
  def stopped = run(numbering(for { b <- fork[Gen]; _ <- if (b) stop[Gen, Unit] else pure[Unit, Gen](()); n <- next[Gen] } yield n))
}
