package okay.effects.tofuinterop

import _root_.cats.Functor
import okay.effects.{Runner, Std}
import okay.effects.catsinterop.Cats
import tofu.{Errors, WithLocal}

/**
 * TOFU'S TYPECLASSES OVER THE EFFECTS, on any implementation (one source for both compilers): code written over
 * `F[_]` with tofu runs on the programs over a row `R`, with `Cats`'s monad.
 *
 *  - `withLocal`: the context a `Reader` anywhere in `R`; `local` its programs `ask`ing the context changed — the
 *    `Reader` handled again from its place by one of `project` of the context asked once, the rest put back in `R`;
 *  - `errors`: the errors an `Error`; `tryHandleWith`, `restore` the `Error` handled from its place, its program put
 *    back in `R`, the recovery in `R` — so a raise in it goes out to the handler outside.
 *
 * Each asks where the effect is (`Split`) and that the rest of the row is in `R` (`Sub`): found for a row named.
 * `val tofu = Tofu(Cats(Machine)); import tofu._`
 */
class Tofu[F <: Runner](val catsInterop: Cats[F]) {
  import catsInterop.fx._
  /** the standard effects, named in full: tofu's classes have an `ask`, an `attempt` of their own */
  private val std = catsInterop.fx.std

  implicit def withLocal[R[_], Rest[_], C](implicit sp: Split[({ type L[x] = Std.Reader[C, x] })#L, R, Rest],
      sub: Sub[Rest, R]): WithLocal[catsInterop.Program[R]#L, C] = new WithLocal[catsInterop.Program[R]#L, C] {
    def functor: Functor[catsInterop.Program[R]#L] = catsInterop.monad[R]
    def context: C ! R = std.ask[C, R]
    def local[A](fa: A ! R)(project: C => C): A ! R =
      std.ask[C, R].flatMap(c => embed[Rest, R, A](std.reader[C](project(c))(fa)))
  }

  implicit def errors[R[_], Rest[_], E](implicit sp: Split[({ type L[x] = Std.Error[E, x] })#L, R, Rest],
      sub: Sub[Rest, R]): Errors[catsInterop.Program[R]#L, E] = new Errors[catsInterop.Program[R]#L, E] {
    def raise[A](err: E): A ! R = std.raise[E, R, A](err)
    def tryHandleWith[A](fa: A ! R)(f: E => Option[A ! R]): A ! R =
      embed[Rest, R, Either[E, A]](std.attempt[E](fa)).flatMap {
        case Right(a) => pure[A, R](a)
        case Left(e) => f(e).getOrElse(raise[A](e))
      }
    def restore[A](fa: A ! R): Option[A] ! R = embed[Rest, R, Either[E, A]](std.attempt[E](fa)).map(_.toOption)
    def lift[A](fa: A ! R): A ! R = fa
  }
}

object Tofu {
  def apply[F <: Runner](catsInterop: Cats[F]): Tofu[F] = new Tofu[F](catsInterop)
}
