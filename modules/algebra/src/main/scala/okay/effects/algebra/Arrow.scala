package okay.effects.algebra

/** a two-argument constructor contravariant in the first, covariant in the second: a function, a Kleisli arrow, a
 * `Fold` (what goes in, what comes out). Optics by profunctors are NOT here: the simple optics fall out of `shape` */
trait Profunctor[P[_, _]] {
  def dimap[A, B, C, D](p: P[A, B])(f: C => A)(g: B => D): P[C, D]
  def lmap[A, B, C](p: P[A, B])(f: C => A): P[C, B] = dimap(p)(f)((b: B) => b)
  def rmap[A, B, D](p: P[A, B])(g: B => D): P[A, D] = dimap(p)((a: A) => a)(g)
}

object Profunctor {
  def apply[P[_, _]](implicit P: Profunctor[P]): Profunctor[P] = P
  implicit val function: Arrow[Function1] = Arrow.function
  /** a fold's input mapped before, its result after; the state stays its own */
  implicit val fold: Profunctor[Fold] = new Profunctor[Fold] {
    def dimap[A, B, C, D](p: Fold[A, B])(f: C => A)(g: B => D): Fold[C, D] = new Fold[C, D] {
      type S = p.S
      def init: S = p.init
      def add(s: S, c: C): S = p.add(s, f(c))
      def end(s: S): D = g(p.end(s))
    }
  }
}

/** composition with an identity */
trait Category[P[_, _]] {
  def id[A]: P[A, A]
  def compose[A, B, C](g: P[B, C], f: P[A, B]): P[A, C]
  def andThen[A, B, C](f: P[A, B], g: P[B, C]): P[A, C] = compose(g, f)
}

/** a Category that lifts functions and runs beside a value it leaves alone: Hughes' arrows */
trait Arrow[P[_, _]] extends Category[P] with Profunctor[P] {
  def lift[A, B](f: A => B): P[A, B]
  def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
  def second[A, B, C](p: P[A, B]): P[(C, A), (C, B)] = {
    val swap = lift[(C, A), (A, C)] { case (c, a) => (a, c) }
    val back = lift[(B, C), (C, B)] { case (b, c) => (c, b) }
    compose(back, compose(first[A, B, C](p), swap))
  }
  /** both, side by side: `***` */
  def split[A, B, C, D](f: P[A, B], g: P[C, D]): P[(A, C), (B, D)] = compose(second[C, D, B](g), first[A, B, C](f))
  /** both on one input: `&&&` */
  def fanout[A, B, C](f: P[A, B], g: P[A, C]): P[A, (B, C)] = compose(split(f, g), lift((a: A) => (a, a)))
  def dimap[A, B, C, D](p: P[A, B])(f: C => A)(g: B => D): P[C, D] = compose(lift(g), compose(p, lift(f)))
}

object Arrow {
  def apply[P[_, _]](implicit P: Arrow[P]): Arrow[P] = P

  implicit val function: Arrow[Function1] = new Arrow[Function1] {
    def id[A]: A => A = a => a
    def compose[A, B, C](g: B => C, f: A => B): A => C = f.andThen(g)
    def lift[A, B](f: A => B): A => B = f
    def first[A, B, C](p: A => B): ((A, C)) => (B, C) = { case (a, c) => (p(a), c) }
  }

  /** `A => F[B]` for a Monad `F`: the arrow of effectful functions, programs' among them */
  final case class Kleisli[F[_], A, B](run: A => F[B])
  implicit def kleisli[F[_]](implicit M: Monad[F]): Arrow[({ type L[A, B] = Kleisli[F, A, B] })#L] =
    new Arrow[({ type L[A, B] = Kleisli[F, A, B] })#L] {
      def id[A]: Kleisli[F, A, A] = Kleisli(a => M.pure(a))
      def compose[A, B, C](g: Kleisli[F, B, C], f: Kleisli[F, A, B]): Kleisli[F, A, C] =
        Kleisli(a => M.flatMap(f.run(a))(g.run))
      def lift[A, B](f: A => B): Kleisli[F, A, B] = Kleisli(a => M.pure(f(a)))
      def first[A, B, C](p: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] = Kleisli { case (a, c) =>
        M.fmap(p.run(a), (b: B) => (b, c))
      }
    }
}
