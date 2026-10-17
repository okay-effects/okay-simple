package okay.effects.cats

import cats.{Monad, StackSafeMonad}
import okay.effects.{Runner, Std}

/**
 * CATS OVER THE EFFECTS, on any implementation `fx` (one source for both compilers):
 *
 *  - `monad[R]`: cats' `Monad` of the programs over the row `R`, stack safe (`StackSafeMonad`, `tailRecM` by
 *    `flatMap`: the implementations run in constant stack) — code written over `F[_]: Monad` runs on them;
 *  - `lowering`: any cats monad as the standard effects' `Lowering` — so `std.lift(ga)` performs a `G[X]` inside a
 *    program and `lower(p)` gives the program back as a `G[A]`: `IO` (cats-effect), `Either`, `Option`.
 *
 * `val cats = Cats(Machine); import cats._`
 */
class Cats[F <: Runner](implicit val fx: F) {
  import fx._

  /** a program over the row `R` as cats sees it: a type constructor of its value */
  type Program[R[_]] = { type L[A] = A ! R }

  implicit def monad[R[_]]: Monad[Program[R]#L] = new StackSafeMonad[Program[R]#L] {
    def pure[A](a: A): A ! R = fx.pure[A, R](a)
    def flatMap[A, B](p: A ! R)(f: A => B ! R): B ! R = fx.flatMap[A, B, R](p)(f)
  }

  implicit def lowering[G[_]](implicit G: Monad[G]): Std.Lowering[G] = new Std.Lowering[G] {
    def pure[A](a: A): G[A] = G.pure(a)
    def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B] = G.flatMap(ga)(f)
  }
}

object Cats {
  def apply()(implicit fx: Runner): Cats[fx.type] = {
    implicit val exact: fx.type = fx
    new Cats[fx.type]
  }
}
