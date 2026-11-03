package okay.effects.algebra

/**
 * COMONADS THAT CARRY A CONTEXT — the instances coeffects are made of (Petricek, Orchard and Mycroft, "Coeffects: a
 * calculus of context-dependent computation", 2014; Uustalu and Vene, "Comonadic notions of computation", 2008): a
 * value in its context, `extract` the value, `coflatMap(f)` f at every point of the context. A coKleisli arrow
 * `W[A] => B` is a computation that needs the context `W` gives.
 */

/** THE HISTORY BEHIND A POINT: the past, in order, and the focus — never empty. `coflatMap(f)` f at every point, each
 * with the history up to it: a decision that reads what came before, made again at every step (an audit) */
final case class History[A](past: Vector[A], focus: A) {
  def extract: A = focus
  def extend[B](f: History[A] => B): History[B] =
    History(past.indices.toVector.map(i => f(History(past.take(i), past(i)))), f(this))
  def map[B](f: A => B): History[B] = History(past.map(f), f(focus))
  /** every point, the focus last */
  def all: Vector[A] = past :+ focus
  /** this history with `next` its focus, this one's focus gone into the past */
  def :+(next: A): History[A] = History(all, next)
}

object History {
  /** the history of these points, the last its focus; none of none */
  def of[A](points: Vector[A]): Option[History[A]] = points.lastOption.map(History(points.init, _))

  implicit val comonad: Comonad[History] = new Comonad[History] {
    def fmap[A, B](a: History[A], f: A => B): History[B] = a.map(f)
    def extract[A](a: History[A]): A = a.focus
    def coflatMap[A, B](a: History[A])(f: History[A] => B): History[B] = a.extend(f)
  }
}

/** A VALUE BESIDE AN ENVIRONMENT: what a computation is given, read where it is needed (the coeffect of implicit
 * parameters, flat: the same context everywhere) */
final case class Env[E, A](env: E, value: A)

object Env {
  implicit def comonad[E]: Comonad[({ type L[X] = Env[E, X] })#L] = new Comonad[({ type L[X] = Env[E, X] })#L] {
    def fmap[A, B](a: Env[E, A], f: A => B): Env[E, B] = Env(a.env, f(a.value))
    def extract[A](a: Env[E, A]): A = a.value
    def coflatMap[A, B](a: Env[E, A])(f: Env[E, A] => B): Env[E, B] = Env(a.env, f(a))
  }
}

/** A STORE: a position in a space, and what is at every position — a cursor over a grid, a document, a window;
 * `coflatMap(f)` f at every position (a cellular automaton's step, a blur) */
final case class Store[S, A](peek: S => A, pos: S) {
  def extract: A = peek(pos)
  def seek(s: S): Store[S, A] = Store(peek, s)
}

object Store {
  implicit def comonad[S]: Comonad[({ type L[X] = Store[S, X] })#L] = new Comonad[({ type L[X] = Store[S, X] })#L] {
    def fmap[A, B](a: Store[S, A], f: A => B): Store[S, B] = Store(a.peek.andThen(f), a.pos)
    def extract[A](a: Store[S, A]): A = a.extract
    def coflatMap[A, B](a: Store[S, A])(f: Store[S, A] => B): Store[S, B] = Store(s => f(a.seek(s)), a.pos)
  }
}

/** THE COFREE COMONAD over a functor `F`: a value and its branches, each a cofree again — a tree of what may follow
 * (`F = List`: futures that branch; `F = Option`: a stream that may end). `coflatMap(f)` f at every subtree: a risk of
 * everything below each point. Lazy in its branches, so an unfolded future is built as far as it is read */
final class Cofree[F[_], A](val head: A, tail0: => F[Cofree[F, A]]) {
  lazy val tail: F[Cofree[F, A]] = tail0
}

object Cofree {
  def apply[F[_], A](head: A, tail: => F[Cofree[F, A]]): Cofree[F, A] = new Cofree(head, tail)

  /** a seed unfolded: its value, its branches by `next` — as far as it is read */
  def unfold[F[_], A](seed: A)(next: A => F[A])(implicit F: Functor[F]): Cofree[F, A] =
    Cofree(seed, F.fmap(next(seed), (a: A) => unfold(a)(next)))

  implicit def comonad[F[_]](implicit F: Functor[F]): Comonad[({ type L[X] = Cofree[F, X] })#L] =
    new Comonad[({ type L[X] = Cofree[F, X] })#L] {
      def fmap[A, B](a: Cofree[F, A], f: A => B): Cofree[F, B] =
        Cofree(f(a.head), F.fmap(a.tail, (t: Cofree[F, A]) => fmap(t, f)))
      def extract[A](a: Cofree[F, A]): A = a.head
      def coflatMap[A, B](a: Cofree[F, A])(f: Cofree[F, A] => B): Cofree[F, B] =
        Cofree(f(a), F.fmap(a.tail, (t: Cofree[F, A]) => coflatMap(t)(f)))
    }
}
