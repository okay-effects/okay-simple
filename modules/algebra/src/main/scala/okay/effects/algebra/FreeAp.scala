package okay.effects.algebra

/** a natural transformation: `F[A]` to `G[A]` for every `A` */
trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

/** a function of every `F[A]` to one `M`: what `FreeAp.analyze` takes */
trait Const[F[_], M] {
  def apply[A](fa: F[A]): M
}

/**
 * THE FREE APPLICATIVE over `F`: a value `A` built from `F`-steps that do not depend on one another's values — so it
 * can be RUN in any Applicative (`foldMap`) and INSPECTED without running (`analyze`): the steps are known before
 * any is taken. A record's fields are the example: decoded into an accumulating validation, listed into a schema.
 * Two constructors; the interpretation is a fold over `Ap`.
 */
sealed trait FreeAp[F[_], A] {
  def foldMap[G[_]](nt: F ~> G)(implicit G: Applicative[G]): G[A]
  /** every step's `M`, combined in order, no step run */
  def analyze[M](f: Const[F, M])(implicit M: Monoid[M]): M
  def map[B](f: A => B): FreeAp[F, B] = FreeAp.Ap[F, A, B](FreeAp.Pure[F, A => B](f), this)
  /** every step carried to `G`, the structure kept: a record's fields each changed, the record the same */
  def hoist[G[_]](nt: F ~> G): FreeAp[G, A]
}

object FreeAp {
  final case class Pure[F[_], A](a: A) extends FreeAp[F, A] {
    def foldMap[G[_]](nt: F ~> G)(implicit G: Applicative[G]): G[A] = G.pure(a)
    def analyze[M](f: Const[F, M])(implicit M: Monoid[M]): M = M.empty
    def hoist[G[_]](nt: F ~> G): FreeAp[G, A] = Pure(a)
  }
  /** `f` applied to the step `fa`'s value */
  final case class Ap[F[_], A, B](f: FreeAp[F, A => B], fa: FreeAp[F, A]) extends FreeAp[F, B] {
    def foldMap[G[_]](nt: F ~> G)(implicit G: Applicative[G]): G[B] = G.app(f.foldMap(nt), fa.foldMap(nt))
    def analyze[M](f0: Const[F, M])(implicit M: Monoid[M]): M = M.combine(f.analyze(f0), fa.analyze(f0))
    def hoist[G[_]](nt: F ~> G): FreeAp[G, B] = Ap(f.hoist(nt), fa.hoist(nt))
  }
  /** one step */
  final case class Lift[F[_], A](fa: F[A]) extends FreeAp[F, A] {
    def foldMap[G[_]](nt: F ~> G)(implicit G: Applicative[G]): G[A] = nt(fa)
    def analyze[M](f: Const[F, M])(implicit M: Monoid[M]): M = f(fa)
    def hoist[G[_]](nt: F ~> G): FreeAp[G, A] = Lift(nt(fa))
  }

  def pure[F[_], A](a: A): FreeAp[F, A] = Pure(a)
  def lift[F[_], A](fa: F[A]): FreeAp[F, A] = Lift(fa)
  /** `FreeAp[F, *]` is itself an Applicative */
  implicit def applicative[F[_]]: Applicative[({ type L[A] = FreeAp[F, A] })#L] =
    new Applicative[({ type L[A] = FreeAp[F, A] })#L] {
      def pure[A](a: A): FreeAp[F, A] = Pure(a)
      def app[A, B](f: FreeAp[F, A => B], a: FreeAp[F, A]): FreeAp[F, B] = Ap(f, a)
    }
}

/** a value `A` or problems `E`, every problem kept: the Applicative that validates a form */
sealed trait Validated[+E, +A] {
  def toEither: Either[E, A] = this match { case Validated.Valid(a) => Right(a); case Validated.Invalid(e) => Left(e) }
}
object Validated {
  final case class Valid[+A](a: A) extends Validated[Nothing, A]
  final case class Invalid[+E](e: E) extends Validated[E, Nothing]
  def fromEither[E, A](e: Either[E, A]): Validated[E, A] = e.fold(Invalid(_), Valid(_))
  /** combines the problems of both sides with the Semigroup — the accumulation the caller chose */
  implicit def applicative[E](implicit E: Semigroup[E]): Applicative[({ type L[A] = Validated[E, A] })#L] =
    new Applicative[({ type L[A] = Validated[E, A] })#L] {
      def pure[A](a: A): Validated[E, A] = Valid(a)
      def app[A, B](f: Validated[E, A => B], a: Validated[E, A]): Validated[E, B] = (f, a) match {
        case (Valid(g), Valid(x)) => Valid(g(x))
        case (Invalid(e1), Invalid(e2)) => Invalid(E.combine(e1, e2))
        case (Invalid(e), _) => Invalid(e)
        case (_, Invalid(e)) => Invalid(e)
      }
    }
}
