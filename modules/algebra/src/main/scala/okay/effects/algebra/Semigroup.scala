package okay.effects.algebra

/** how two values of `A` combine into one; the CALLER chooses what combining means: concatenation for a form's
 * problems, a count for a sampler, a map of field to problems for an API */
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit S: Semigroup[A]): Semigroup[A] = S
  def of[A](f: (A, A) => A): Semigroup[A] = new Semigroup[A] { def combine(x: A, y: A): A = f(x, y) }
  /** every Monoid is one */
  implicit def fromMonoid[A](implicit M: Monoid[A]): Semigroup[A] = M
}

/** a Semigroup with a neutral element */
trait Monoid[A] extends Semigroup[A] {
  def empty: A
  def combineAll(xs: IterableOnce[A]): A = xs.iterator.foldLeft(empty)(combine)
}

object Monoid {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M

  /** make one from its two parts */
  def of[A](zero: A)(f: (A, A) => A): Monoid[A] = new Monoid[A] {
    def empty: A = zero
    def combine(x: A, y: A): A = f(x, y)
  }

  implicit def vector[A]: Monoid[Vector[A]] = of(Vector.empty[A])(_ ++ _)
  implicit def list[A]: Monoid[List[A]] = of(List.empty[A])(_ ++ _)
  implicit def set[A]: Monoid[Set[A]] = of(Set.empty[A])(_ ++ _)
  implicit def map[K, V](implicit V: Semigroup[V]): Monoid[Map[K, V]] =
    of(Map.empty[K, V])((x, y) =>
      y.foldLeft(x) { case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(v)(V.combine(_, v))) })
  implicit def option[A](implicit A: Semigroup[A]): Monoid[Option[A]] =
    of(Option.empty[A])((x, y) =>
      (x, y) match { case (Some(a), Some(b)) => Some(A.combine(a, b)); case (a, None) => a; case (None, b) => b })
  implicit val string: Monoid[String] = of("")(_ + _)
  implicit val unit: Monoid[Unit] = of(())((_, _) => ())
  val all: Monoid[Boolean] = of(true)(_ && _)
  val any: Monoid[Boolean] = of(false)(_ || _)
  /** every Numeric is a Group under addition */
  implicit def numeric[N](implicit N: Numeric[N]): Group[N] = new Group[N] {
    def empty: N = N.zero
    def combine(x: N, y: N): N = N.plus(x, y)
    def inverse(a: N): N = N.negate(a)
  }
}

/** a Monoid with inverses */
trait Group[A] extends Monoid[A] {
  def inverse(a: A): A
}
