package okay.effects.algebra

import okay.effects.Effects

/**
 * THE CLASSES' INSTANCES FOR PROGRAMS, over any implementation `fx`: a program `A ! R` is a Monad, with a stack-safe
 * `tailRecM` (every implementation runs programs as a loop, so the recursion is deferred); with `Choose` in its row,
 * a MonadPlus; `Delimited`'s `Cps`, a ParaMonad. And the generic words the classes exist for — `traverse`,
 * `sequence`, `replicateA` — over any instance.
 */
class Algebra[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation
   * to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  /** programs of the row `R`, as a Monad with a stack-safe loop */
  def monad[R[_]]: Monad[({ type L[A] = A ! R })#L] with TailRecM[({ type L[A] = A ! R })#L] =
    new Monad[({ type L[A] = A ! R })#L] with TailRecM[({ type L[A] = A ! R })#L] {
      def pure[A](a: A): A ! R = fx.pure[A, R](a)
      def flatMap[A, B](p: A ! R)(f: A => B ! R): B ! R = fx.flatMap[A, B, R](p)(f)
      override def fmap[A, B](p: A ! R, f: A => B): B ! R = fx.map[A, B, R](p)(f)
      def tailRecM[A, B](a: A)(f: A => Either[A, B] ! R): B ! R =
        fx.flatMap[Either[A, B], B, R](f(a)) {
          case Left(next) => tailRecM(next)(f)
          case Right(b) => fx.pure[B, R](b)
        }
    }

  /** programs of a row with `Choose`: `empty` fails, `append` is a choice between the two */
  def monadPlus[R[_]](
    implicit c: In[Choose, R]): MonadPlus[({ type L[A] = A ! R })#L] with TailRecM[({ type L[A] = A ! R })#L] =
    new MonadPlus[({ type L[A] = A ! R })#L] with TailRecM[({ type L[A] = A ! R })#L] {
      def pure[A](a: A): A ! R = fx.pure[A, R](a)
      def flatMap[A, B](p: A ! R)(f: A => B ! R): B ! R = fx.flatMap[A, B, R](p)(f)
      override def fmap[A, B](p: A ! R, f: A => B): B ! R = fx.map[A, B, R](p)(f)
      def empty[A]: A ! R = among[A, R](Nil)
      def append[A](x: A ! R, y: => A ! R): A ! R =
        fx.flatMap[Boolean, A, R](among[Boolean, R](List(true, false)))(first => if (first) x else y)
      def tailRecM[A, B](a: A)(f: A => Either[A, B] ! R): B ! R =
        fx.flatMap[Either[A, B], B, R](f(a)) {
          case Left(next) => tailRecM(next)(f)
          case Right(b) => fx.pure[B, R](b)
        }
    }

  // ---- THE WORDS SHAPED FOR PROGRAMS. `A ! R` is `![A, R]`, and partial unification abstracts the LAST parameter, so
  // against an implementation left abstract the generic `traverse[F, A, B]` reads `F` as `[R] =>> A ! R` — reversed.
  // These take the row and need no inference; on a concrete implementation the generic words work too
  def traverse[A, B, R[_]](xs: Seq[A])(f: A => B ! R): Vector[B] ! R =
    syntax.traverse[({ type L[X] = X ! R })#L, A, B](xs)(f)(monad[R])
  def sequence[A, R[_]](xs: Seq[A ! R]): Vector[A] ! R = syntax.sequence[({ type L[X] = X ! R })#L, A](xs)(monad[R])
  def replicateA[A, R[_]](n: Int)(p: A ! R): Vector[A] ! R =
    syntax.replicateA[({ type L[X] = X ! R })#L, A](n)(p)(monad[R])
  /** a loop over programs: from `a`, on from a `Left`, done at a `Right`; constant stack, the implementation's run is a
   * loop */
  def loop[A, B, R[_]](a: A)(f: A => Either[A, B] ! R): B ! R = monad[R].tailRecM(a)(f)
  /** the applicative operators on programs */
  implicit final class ProgramOps[A, R[_]](p: A ! R) {
    /** then `q`, keep the right */
    def *>[B](q: => B ! R): B ! R = fx.flatMap[A, B, R](p)((_: A) => q)
    /** then `q`, keep the left */
    def <*[B](q: => B ! R): A ! R = fx.flatMap[A, A, R](p)((a: A) => fx.map[B, A, R](q)((_: B) => a))
    def >>[B](q: => B ! R): B ! R = fx.flatMap[A, B, R](p)((_: A) => q)
    def void: Unit ! R = fx.map[A, Unit, R](p)((_: A) => ())
  }

  /** delimited control with answer-type modification, `Cps[A, B, C, R]`, as a parameterised monad: the answer `B` to
   * `C` */
  def paraMonad[R[_]]: ParaMonad[({ type L[A, S, T] = fx.delimited.Cps[A, S, T, R] })#L] =
    new ParaMonad[({ type L[A, S, T] = fx.delimited.Cps[A, S, T, R] })#L] {
      def pure[A, S](a: A): fx.delimited.Cps[A, S, S, R] = fx.delimited.pure[A, S, R](a)
      def flatMap[A, B, S, S2, T](m: fx.delimited.Cps[A, S, T, R])(
        f: A => fx.delimited.Cps[B, S2, S, R]): fx.delimited.Cps[B, S2, T, R] =
        fx.delimited.flatMap[A, B, S, T, S2, R](m)(f)
    }
}

object Algebra {
  def apply()(implicit fx: Effects): Algebra[fx.type] = {
    implicit val exact: fx.type = fx
    new Algebra[fx.type]
  }
}

/** THE GENERIC WORDS over any instance, and the classes' operations as methods on a value — present only where an
 * instance is. `import okay.effects.algebra.syntax._` */
trait Words {
  /** effectful map over a sequence, effects in order, results collected */
  def traverse[F[_], A, B](xs: Seq[A])(f: A => F[B])(implicit M: Applicative[F]): F[Vector[B]] =
    xs.foldLeft(M.pure(Vector.empty[B])) { (acc, a) => M.app(M.fmap(acc, (s: Vector[B]) => (b: B) => s :+ b), f(a)) }
  /** a sequence of computations into a computation of the sequence */
  def sequence[F[_], A](xs: Seq[F[A]])(implicit M: Applicative[F]): F[Vector[A]] = traverse[F, F[A], A](xs)(identity)
  /** the same computation n times, results collected */
  def replicateA[F[_], A](n: Int)(fa: F[A])(implicit M: Applicative[F]): F[Vector[A]] = sequence[F, A](Seq.fill(n)(fa))
  /** `f` over a sequence, for its effects only */
  def traverse_[F[_], A, B](xs: Seq[A])(f: A => F[B])(implicit M: Applicative[F]): F[Unit] =
    xs.foldLeft(M.pure(())) { (acc, a) => M.app(M.fmap(acc, (_: Unit) => (_: B) => ()), f(a)) }
  /** a loop: from `a`, on from a `Left`, done at a `Right` — stack-safe, the carrier's */
  def loop[F[_], A, B](a: A)(f: A => F[Either[A, B]])(implicit R: TailRecM[F]): F[B] = R.tailRecM(a)(f)

  /** Kleisli composition, `>=>` */
  implicit final class KleisliSyntax[F[_], A, B](f: A => F[B])(implicit M: Monad[F]) {
    def >=>[C](g: B => F[C]): A => F[C] = a => M.flatMap(f(a))(g)
  }
  implicit final class FunctorSyntax[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def fmap[B](f: A => B): F[B] = F.fmap(fa, f)
    def as[B](b: B): F[B] = F.fmap(fa, (_: A) => b)
    def void: F[Unit] = F.fmap(fa, (_: A) => ())
  }
  implicit final class ApplicativeSyntax[F[_], A](fa: F[A])(implicit F: Applicative[F]) {
    /** sequence, keep the right */
    def *>[B](fb: F[B]): F[B] = F.app(F.fmap(fa, (_: A) => (b: B) => b), fb)
    /** sequence, keep the left */
    def <*[B](fb: F[B]): F[A] = F.app(F.fmap(fa, (a: A) => (_: B) => a), fb)
    def product[B](fb: F[B]): F[(A, B)] = F.product(fa, fb)
  }
  implicit final class ApSyntax[F[_], A, B](f: F[A => B])(implicit F: Applicative[F]) {
    /** the idiom bracket's spelling: `pure(f) <*> fa <*> fb` */
    def <*>(a: F[A]): F[B] = F.app(f, a)
  }
  implicit final class SelectiveSyntax[F[_], A, B](fe: F[Either[A, B]])(implicit F: Selective[F]) {
    def select(f: => F[A => B]): F[B] = F.select(fe, f)
    def branch[C](fa: => F[A => C])(fb: => F[B => C]): F[C] = F.branch(fe)(fa)(fb)
  }
  implicit final class SelectiveBoolSyntax[F[_]](cond: F[Boolean])(implicit F: Selective[F]) {
    def ifS[A](t: => F[A])(e: => F[A]): F[A] = F.ifS(cond)(t)(e)
    def whenS(body: => F[Unit]): F[Unit] = F.whenS(cond)(body)
    def unlessS(body: => F[Unit]): F[Unit] = F.ifS(cond)(F.pure(()))(body)
  }
  implicit final class MonadSyntax[F[_], A](fa: F[A])(implicit M: Monad[F]) {
    def >>=[B](f: A => F[B]): F[B] = M.flatMap(fa)(f)
    def >>[B](fb: => F[B]): F[B] = M.flatMap(fa)((_: A) => fb)
  }
  implicit final class AlternativeSyntax[F[_], A](x: F[A])(implicit F: Alternative[F]) {
    def <|>(y: => F[A]): F[A] = F.append(x, y)
  }
  implicit final class ComonadSyntax[F[_], A](a: F[A])(implicit F: Comonad[F]) {
    def extract: A = F.extract(a)
    def coflatMap[B](f: F[A] => B): F[B] = F.coflatMap(a)(f)
  }
  implicit final class ArrowSyntax[P[_, _], A, B](f: P[A, B])(implicit P: Arrow[P]) {
    def >>>[C](g: P[B, C]): P[A, C] = P.andThen(f, g)
    def <<<[C](g: P[C, A]): P[C, B] = P.compose(f, g)
    def ***[C, D](g: P[C, D]): P[(A, C), (B, D)] = P.split(f, g)
    def &&&[C](g: P[A, C]): P[A, (B, C)] = P.fanout(f, g)
    def dimap[C, D](l: C => A)(r: B => D): P[C, D] = P.dimap(f)(l)(r)
  }
  implicit final class SemigroupSyntax[A](x: A)(implicit S: Semigroup[A]) {
    def |+|(y: A): A = S.combine(x, y)
  }
  implicit final class FoldableSyntax[F[_], A](fa: F[A])(implicit F: Foldable[F]) {
    def foldWith[R](f: Fold[A, R]): R = F.fold(fa)(f)
    def foldMap[W: Monoid](f: A => W): W = F.foldMap(fa)(f)
  }
}

object syntax extends Words
