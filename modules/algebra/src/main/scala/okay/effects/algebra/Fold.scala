package okay.effects.algebra

/**
 * THE ALGEBRA OF A LEFT FOLD: a start and a step. Weaker than a Monoid — no combine of two `S` is asked — so a count,
 * a sum, a last value, a hash, a sink all fit. One algebra, every consumer: a `Foldable` runs it over its elements.
 */
trait Fold[-A, R] {
  /** the state, the fold's own; the result `R` is `end` of it */
  type S
  def init: S
  def add(s: S, a: A): S
  def end(s: S): R
  def run(as: IterableOnce[A]): R = end(as.iterator.foldLeft(init)(add))
}

object Fold {
  /** a fold whose state is its result */
  def apply[A, R](z: R)(f: (R, A) => R): Fold[A, R] = new Fold[A, R] {
    type S = R
    def init: R = z
    def add(s: R, a: A): R = f(s, a)
    def end(s: R): R = s
  }
  /** a fold of a state of its own, finished by `finish` */
  def of[A, S0, R](z: S0)(f: (S0, A) => S0)(finish: S0 => R): Fold[A, R] = new Fold[A, R] {
    type S = S0
    def init: S0 = z
    def add(s: S0, a: A): S0 = f(s, a)
    def end(s: S0): R = finish(s)
  }
  /** the default: collect in order */
  implicit def collect[A]: Fold[A, Vector[A]] = apply(Vector.empty[A])(_ :+ _)
  /** every Monoid is one */
  def ofMonoid[W](implicit M: Monoid[W]): Fold[W, W] = apply(M.empty)(M.combine)
  def foldMap[A, W](f: A => W)(implicit M: Monoid[W]): Fold[A, W] = apply(M.empty)((w, a) => M.combine(w, f(a)))
  def sum[N](implicit N: Numeric[N]): Fold[N, N] = apply(N.zero)(N.plus)
  def count[A]: Fold[A, Long] = apply(0L)((n, _) => n + 1)
  def first[A]: Fold[A, Option[A]] = apply(Option.empty[A])((s, a) => s.orElse(Some(a)))
  def last[A]: Fold[A, Option[A]] = apply(Option.empty[A])((_, a) => Some(a))
  def exists[A](p: A => Boolean): Fold[A, Boolean] = apply(false)((s, a) => s || p(a))
  def forall[A](p: A => Boolean): Fold[A, Boolean] = apply(true)((s, a) => s && p(a))
  def mean: Fold[Double, Double] = of[Double, (Double, Long), Double]((0.0, 0L)) { case ((sum, n), x) =>
    (sum + x, n + 1)
  } { case (sum, n) => if (n == 0) 0.0 else sum / n }

  /** two folds in ONE pass: `Fold[A, *]` is an Applicative (the state a pair) — `(sum, count).mapN(_ / _)` the mean */
  implicit def applicative[A]: Applicative[({ type L[R] = Fold[A, R] })#L] =
    new Applicative[({ type L[R] = Fold[A, R] })#L] {
      def pure[R](r: R): Fold[A, R] = of[A, Unit, R](())((_, _) => ())(_ => r)
      def app[R, T](f: Fold[A, R => T], x: Fold[A, R]): Fold[A, T] = new Fold[A, T] {
        type S = (f.S, x.S)
        def init: S = (f.init, x.init)
        def add(s: S, a: A): S = (f.add(s._1, a), x.add(s._2, a))
        def end(s: S): T = f.end(s._1)(x.end(s._2))
      }
    }
}

/** a fold that may STOP: `done` says when, `end` turns the state into the result */
trait FoldUntil[-A, S, R] {
  def init: S
  def add(s: S, a: A): S
  def done(s: S): Boolean
  def end(s: S): R
}

object FoldUntil {
  def apply[A, S, R](z: S)(f: (S, A) => S)(stop: S => Boolean)(finish: S => R): FoldUntil[A, S, R] =
    new FoldUntil[A, S, R] {
      def init: S = z
      def add(s: S, a: A): S = f(s, a)
      def done(s: S): Boolean = stop(s)
      def end(s: S): R = finish(s)
    }
  /** every Fold is one that never stops */
  def ofFold[A, R](fo: Fold[A, R]): FoldUntil[A, fo.S, R] = apply[A, fo.S, R](fo.init)(fo.add)(_ => false)(fo.end)
  def find[A](p: A => Boolean): FoldUntil[A, Option[A], Option[A]] =
    apply[A, Option[A], Option[A]](Option.empty[A])((s, a) => if (s.isDefined) s else if (p(a)) Some(a) else None)(
      _.isDefined)(identity)
  def headOption[A]: FoldUntil[A, Option[A], Option[A]] = find(_ => true)
  def take[A](n: Int): FoldUntil[A, Vector[A], Vector[A]] =
    apply[A, Vector[A], Vector[A]](Vector.empty[A])((s, a) => if (s.size < n) s :+ a else s)(_.size >= n)(identity)
}

/** a structure its elements can be folded out of, in order */
trait Foldable[F[_]] {
  def fold[A, R](fa: F[A])(f: Fold[A, R]): R
  /** stopping when the fold says so — a lazy structure is walked no further */
  def foldUntil[A, S, R](fa: F[A])(fo: FoldUntil[A, S, R]): R
  def foldLeft[A, S](fa: F[A])(z: S)(f: (S, A) => S): S = fold(fa)(Fold(z)(f))
  def foldMap[A, W](fa: F[A])(f: A => W)(implicit M: Monoid[W]): W = fold(fa)(Fold.foldMap(f))
  def toList[A](fa: F[A]): List[A] = fold(fa)(Fold.collect[A]).toList
  def size[A](fa: F[A]): Long = fold(fa)(Fold.count)
  def exists[A](fa: F[A])(p: A => Boolean): Boolean = foldUntil(fa)(FoldUntil.find(p)).isDefined
  def forall[A](fa: F[A])(p: A => Boolean): Boolean = !exists(fa)(a => !p(a))
  def find[A](fa: F[A])(p: A => Boolean): Option[A] = foldUntil(fa)(FoldUntil.find(p))
}

object Foldable {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  /** every Scala collection, by its iterator: lazily as far as a stopping fold asks */
  implicit def iterableOnce[F[X] <: IterableOnce[X]]: Foldable[F] = new Foldable[F] {
    def fold[A, R](fa: F[A])(f: Fold[A, R]): R = f.run(fa)
    def foldUntil[A, S, R](fa: F[A])(fo: FoldUntil[A, S, R]): R = {
      val it = fa.iterator; var s = fo.init
      while (!fo.done(s) && it.hasNext) s = fo.add(s, it.next())
      fo.end(s)
    }
  }
  implicit val option: Foldable[Option] = new Foldable[Option] {
    def fold[A, R](fa: Option[A])(f: Fold[A, R]): R = f.end(fa.fold(f.init)(a => f.add(f.init, a)))
    def foldUntil[A, S, R](fa: Option[A])(fo: FoldUntil[A, S, R]): R =
      fo.end(fa.fold(fo.init)(a => if (fo.done(fo.init)) fo.init else fo.add(fo.init, a)))
  }
}
