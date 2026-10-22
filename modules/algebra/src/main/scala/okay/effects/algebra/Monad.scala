package okay.effects.algebra

import scala.annotation.{implicitNotFound, tailrec}

/**
 * THE CLASSES OF COMPUTATIONS — Functor, Applicative, Selective, Monad, TailRecM, Alternative, MonadPlus, Comonad, and
 * ParaMonad, the parameterised one — written once for both compilers, from okay's okay2 (its Scala 2.13 core), with
 * what was the platform's left out. Instances for the standard types live in the companions of the classes' BASE
 * (`Functor`'s), where a search for any class of the hierarchy looks; instances for PROGRAMS, `A ! R`, are
 * `Algebra`'s, per implementation.
 *
 * Literature: Wadler, "Monads for functional programming" (1995); McBride & Paterson, "Applicative programming with
 * effects" (2008); Mokhov, Lukyanov, Marlow & Dimino, "Selective applicative functors" (2019); Atkey, "Parameterised
 * notions of computation" (2009); Uustalu & Vene, "Comonadic notions of computation" (2008).
 */

/** map a function over the structure */
@implicitNotFound(
  "no Functor[${F}]. The instance for programs is Algebra's `monad[R]` (okay.effects.algebra); for your own type, " +
    "give one in ITS companion.")
trait Functor[F[_]] {
  def fmap[A, B](a: F[A], f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  /** `Option` is a Monad, globally; HERE because the implicit scope of `Monad[Option]` includes the companions of
   * every base class of the queried class, and `Functor` is the base of them all */
  implicit val option: Monad[Option] with TailRecM[Option] = new Monad[Option] with TailRecM[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    override def fmap[A, B](a: Option[A], f: A => B): Option[B] = a.map(f)
    def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] = a.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = {
      @tailrec def loop(s: A): Option[B] = f(s) match {
        case None => None
        case Some(Left(next)) => loop(next)
        case Some(Right(b)) => Some(b)
      }
      loop(a)
    }
  }

  implicit def either[E]: Monad[({ type L[X] = Either[E, X] })#L] with TailRecM[({ type L[X] = Either[E, X] })#L] =
    new Monad[({ type L[X] = Either[E, X] })#L] with TailRecM[({ type L[X] = Either[E, X] })#L] {
      def pure[A](a: A): Either[E, A] = Right(a)
      override def fmap[A, B](a: Either[E, A], f: A => B): Either[E, B] = a.map(f)
      def flatMap[A, B](a: Either[E, A])(f: A => Either[E, B]): Either[E, B] = a.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = {
        @tailrec def loop(s: A): Either[E, B] = f(s) match {
          case Left(e) => Left(e)
          case Right(Left(next)) => loop(next)
          case Right(Right(b)) => Right(b)
        }
        loop(a)
      }
    }

  implicit val list: MonadPlus[List] with TailRecM[List] = new MonadPlus[List] with TailRecM[List] {
    def pure[A](a: A): List[A] = List(a)
    def empty[A]: List[A] = Nil
    override def fmap[A, B](a: List[A], f: A => B): List[B] = a.map(f)
    def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a.flatMap(f)
    def append[A](x: List[A], y: => List[A]): List[A] = x ++ y
    /** depth-first over every branch, an explicit stack of the branches still to expand */
    def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      val out = List.newBuilder[B]
      @tailrec def loop(stack: List[List[Either[A, B]]]): Unit = stack match {
        case Nil => ()
        case Nil :: rest => loop(rest)
        case (Right(b) :: tail) :: rest => out += b; loop(tail :: rest)
        case (Left(s) :: tail) :: rest => loop(f(s) :: tail :: rest)
      }
      loop(f(a) :: Nil); out.result()
    }
  }

  implicit val vector: MonadPlus[Vector] = new MonadPlus[Vector] {
    def pure[A](a: A): Vector[A] = Vector(a)
    def empty[A]: Vector[A] = Vector.empty
    override def fmap[A, B](a: Vector[A], f: A => B): Vector[B] = a.map(f)
    def flatMap[A, B](a: Vector[A])(f: A => Vector[B]): Vector[B] = a.flatMap(f)
    def append[A](x: Vector[A], y: => Vector[A]): Vector[A] = x ++ y
  }

  /** the lazy list is the canonical MonadPlus: the empty list is failure, appending is concatenation */
  implicit val lazyList: MonadPlus[LazyList] with TailRecM[LazyList] = new MonadPlus[LazyList] with TailRecM[LazyList] {
    def pure[A](a: A): LazyList[A] = LazyList(a)
    def empty[A]: LazyList[A] = LazyList.empty
    def flatMap[A, B](a: LazyList[A])(f: A => LazyList[B]): LazyList[B] = a.flatMap(f)
    def append[A](x: LazyList[A], y: => LazyList[A]): LazyList[A] = x #::: y
    /** depth-first over every branch, LAZILY: an explicit stack of the branches still to expand, and a tail loop
     * that expands until the next `Right` and emits it with the rest suspended. A run of `Left`s costs no frames */
    def tailRecM[A, B](a: A)(f: A => LazyList[Either[A, B]]): LazyList[B] = {
      @tailrec def next(stack: List[LazyList[Either[A, B]]]): LazyList[B] = stack match {
        case Nil => LazyList.empty
        case head :: rest =>
          if (head.isEmpty) next(rest)
          else head.head match {
            case Right(b) => b #:: go(head.tail :: rest)
            case Left(s) => next(f(s) :: head.tail :: rest)
          }
      }
      def go(stack: List[LazyList[Either[A, B]]]): LazyList[B] = LazyList.empty #::: next(stack)
      go(f(a) :: Nil)
    }
  }

  /** a bare value is its own trivial context: `Id[A] = A` — asked for by name, `Monad[Id]`, never put on every value */
  implicit val id: Monad[Id] with Comonad[Id] with TailRecM[Id] = new Monad[Id] with Comonad[Id] with TailRecM[Id] {
    def pure[A](a: A): A = a
    override def fmap[A, B](a: A, f: A => B): B = f(a)
    def flatMap[A, B](a: A)(f: A => B): B = f(a)
    def extract[A](a: A): A = a
    def coflatMap[A, B](a: A)(f: A => B): B = f(a)
    def tailRecM[A, B](a: A)(f: A => Either[A, B]): B = {
      @tailrec def loop(s: A): B = f(s) match { case Left(next) => loop(next); case Right(b) => b }
      loop(a)
    }
  }
}

/** lift values and apply lifted functions; `fmap` derives from `pure` and `app` */
@implicitNotFound(
  "no Applicative[${F}]. The instance for programs is Algebra's `monad[R]` (okay.effects.algebra); Option's, " +
    "Either's, List's are Functor's companion's.")
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def app[A, B](f: F[A => B], a: F[A]): F[B]
  override def fmap[A, B](a: F[A], f: A => B): F[B] = app(pure(f), a)
  /** both, in order, the pair */
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = app(fmap(fa, (a: A) => (b: B) => (a, b)), fb)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = app(fmap(fa, (a: A) => (b: B) => f(a, b)), fb)
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

/**
 * Selective applicative functors (Mokhov et al. 2019): declare both branches statically, run at most one — between
 * Applicative (all effects known, all run) and Monad (continuations opaque).
 */
trait Selective[F[_]] extends Applicative[F] {
  /** BY NAME, and in Scala that is the whole point of the class: by value the skipped branch would still RUN and
   * only its result would be dropped */
  def select[A, B](fe: F[Either[A, B]], f: => F[A => B]): F[B]

  def branch[A, B, C](fe: F[Either[A, B]])(fa: => F[A => C])(fb: => F[B => C]): F[C] =
    select[B, C](
      select[A, Either[B, C]](
        fmap(fe, (e: Either[A, B]) => e.map(b => Left(b): Either[B, C])),
        fmap(fa, (g: A => C) => g.andThen(c => Right(c): Either[B, C]))),
      fb)

  /** `branch` sends Left to its FIRST argument, so true must become Left */
  def ifS[A](x: F[Boolean])(t: => F[A])(e: => F[A]): F[A] =
    branch[Unit, Unit, A](fmap(x, (b: Boolean) => if (b) Left(()) else Right(())))(
      fmap(t, (a: A) => (_: Unit) => a))(fmap(e, (a: A) => (_: Unit) => a))
  def whenS(cond: F[Boolean])(body: => F[Unit]): F[Unit] = ifS(cond)(body)(pure(()))
}

/** sequence computations; `fmap`, `app` and `select` all derive from `flatMap` by the laws */
@implicitNotFound(
  "no Monad[${F}]. The instance for programs is Algebra's `monad[R]` (okay.effects.algebra); Option's, Either's, " +
    "List's are Functor's companion's.")
trait Monad[F[_]] extends Selective[F] {
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  override def fmap[A, B](a: F[A], f: A => B): F[B] = flatMap(a)((x: A) => pure(f(x)))

  // NOT `flatMap(a)(x => app(f, pure(x)))`: that recursion never grounds (app of a pure argument is again app)
  override def app[A, B](f: F[A => B], a: F[A]): F[B] = flatMap(f)((g: A => B) => fmap(a, g))

  override def select[A, B](e: F[Either[A, B]], f: => F[A => B]): F[B] =
    flatMap(e)(_.fold((a: A) => fmap(f, (g: A => B) => g(a)), (b: B) => pure(b)))

  def flatten[A](m: F[F[A]]): F[A] = flatMap(m)(identity)
}

object Monad {
  def apply[F[_]](implicit M: Monad[F]): Monad[F] = M
}

/**
 * A stack-safe loop: run `f` from `a`, go on from a `Left`, answer a `Right`. PROVIDED BY THE CARRIER, NEVER DERIVED
 * FROM `flatMap`: an eager carrier's `flatMap` calls its continuation before it returns, so no wrapper turns `flatMap`
 * recursion into a loop without the carrier's help (Freeman, "Stack Safety for Free", 2015). A monad without an
 * instance has no `tailRecM`: a compile error, never an overflow.
 */
@implicitNotFound(
  "no TailRecM[${F}]: a stack-safe loop is the carrier's to provide. A carrier whose flatMap does not call its " +
    "continuation before returning may say so: implicit val t: TailRecM[${F}] = TailRecM.deferring. An eager one " +
    "writes its own loop.")
trait TailRecM[F[_]] {
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}

object TailRecM {
  def apply[F[_]](implicit R: TailRecM[F]): TailRecM[F] = R
  /** the standard carriers' loops are their Monad instances' (`Functor`'s companion), named here too, because a search
   * for `TailRecM[Option]` looks in THIS companion and `TailRecM` is no base of `Functor` */
  implicit val option: TailRecM[Option] = Functor.option
  implicit def either[E]: TailRecM[({ type L[X] = Either[E, X] })#L] = Functor.either[E]
  implicit val list: TailRecM[List] = Functor.list
  implicit val lazyList: TailRecM[LazyList] = Functor.lazyList
  implicit val id: TailRecM[Id] = Functor.id

  /** the `flatMap` recursion, for a carrier whose `flatMap` does NOT call its continuation before returning (every
   * implementation of okay-effects, which run programs as loops): the recursive call is a value the carrier's own
   * run reaches, so no host frame is held across iterations. A claim made at the instance, checked by a depth test */
  def deferring[F[_]](implicit M: Monad[F]): TailRecM[F] = new TailRecM[F] {
    def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
      M.flatMap(f(a)) {
        case Left(next) => tailRecM(next)(f)
        case Right(b) => M.pure(b)
      }
  }
}

/** choice with a neutral element */
trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A]
  def append[A](x: F[A], y: => F[A]): F[A]
}

/** a Monad that is also an Alternative, under the traditional names */
@implicitNotFound(
  "no MonadPlus[${F}]. List's, Vector's and LazyList's are Functor's companion's; a program's needs Choose in its " +
    "row: Algebra's `monadPlus[R]`.")
trait MonadPlus[F[_]] extends Alternative[F] with Monad[F] {
  def mzero[A]: F[A] = empty[A]
  def mplus[A](x: F[A], y: => F[A]): F[A] = append(x, y)
  /** the branch dies here unless `p` */
  def guard(p: Boolean): F[Unit] = if (p) pure(()) else empty[Unit]
}

object MonadPlus {
  def apply[F[_]](implicit M: MonadPlus[F]): MonadPlus[F] = M
}

/** extract a value from a context */
trait Comonad[F[_]] extends Functor[F] {
  def extract[A](a: F[A]): A
  def coflatMap[A, B](a: F[A])(f: F[A] => B): F[B]
}

object Comonad {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F
}

/**
 * Robert Atkey. Parameterised notions of computation (2009). `M[A, S, R]` computes `A` while changing a state from
 * `S` to `R` — indexed by an arrow `S -> R` in a category of states. Delimited control with answer-type modification
 * is one: `Cps[A, B, C, R]` of okay-effects' `Delimited`, the answer `B` changed to `C` (`Algebra.paraMonad`).
 */
trait ParaMonad[M[_, _, _]] {
  /** identity: R -> R */
  def pure[A, R](a: A): M[A, R, R]
  /** composition: (S2 -> S) then (S -> R) = S2 -> R */
  def flatMap[A, B, S, S2, R](m: M[A, S, R])(f: A => M[B, S2, S]): M[B, S2, R]
  def map[A, B, S, R](m: M[A, S, R])(f: A => B): M[B, S, R] = flatMap(m)((a: A) => pure[B, S](f(a)))
  def flatten[A, S, S2, R](m: M[M[A, S, S2], S2, R]): M[A, S, R] = flatMap(m)((x: M[A, S, S2]) => x)
}

object ParaMonad {
  def apply[M[_, _, _]](implicit P: ParaMonad[M]): ParaMonad[M] = P

  /** every parameterised monad is a family of ordinary monads, one on each diagonal `M[*, R, R]` — asked for by
   * name, since no call site can partially unify `M[A, R, R]` as `F[A]` */
  def diagonal[M[_, _, _], R](implicit P: ParaMonad[M]): Monad[({ type L[A] = M[A, R, R] })#L] =
    new DiagonalMonad[M, R](P)
}

final class DiagonalMonad[M[_, _, _], R](val P: ParaMonad[M]) extends Monad[({ type L[A] = M[A, R, R] })#L] {
  def pure[A](a: A): M[A, R, R] = P.pure[A, R](a)
  def flatMap[A, B](m: M[A, R, R])(f: A => M[B, R, R]): M[B, R, R] = P.flatMap[A, B, R, R, R](m)(f)
}
