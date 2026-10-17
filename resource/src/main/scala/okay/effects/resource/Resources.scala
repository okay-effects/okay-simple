package okay.effects.resource

import okay.effects.Effects

/** A SCOPE'S OPERATION: `Defer(fin)`, the program `fin` (`P`, of the row outside the scope) run when the scope ends.
 * In a row `({ type L[x] = Scope[P, x] })#L` */
sealed trait Scope[P, X]
object Scope {
  final case class Defer[P, X](fin: P, answer: Unit => X) extends Scope[P, X]
}

/**
 * RESOURCES over any implementation `fx`: `scoped(p)` runs `p`, then what `p` deferred, the last first — at the end
 * of `p`, its value kept. `bracket(acquire)(release)(use)`: a resource acquired, its release deferred, used.
 *
 * An error handled inside the scope (`scoped(attempt[E](p))`) ends `p` with its `Left`, the scope's end; one handled
 * outside drops the rest of the scope, its finalizers with it — so handle errors inside. A `k` resumed twice inside
 * the scope ends it twice: each end runs the finalizers deferred before it, so for programs that resume once.
 */
class Resources[F <: Effects](val fx: F) {
  import fx._

  /** `fin`, a program of the row outside the scope `Rest`, deferred to the scope's end */
  def defer[Rest[_], R[_]](fin: Unit ! Rest)(implicit in: In[({ type L[x] = Scope[Unit ! Rest, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Scope[Unit ! Rest, x] })#L].apply[R, Unit](Scope.Defer[Unit ! Rest, Unit](fin, u => u))

  /** acquired, its release deferred, used */
  def bracket[A, B, Rest[_], R[_]](acquire: A ! R)(release: A => Unit ! Rest)(use: A => B ! R)(
      implicit in: In[({ type L[x] = Scope[Unit ! Rest, x] })#L, R]): B ! R =
    acquire.flatMap(a => defer[Rest, R](release(a)).flatMap(_ => use(a)))

  /** `p`, then its finalizers, the last deferred first */
  def scoped[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Scope[Unit ! Rest, x] })#L, R, Rest]): A ! Rest =
    handle[({ type L[x] = Scope[Unit ! Rest, x] })#L, List[Unit ! Rest], R, Rest, A, A](p, Nil,
      new StatefulTailHandler[({ type L[x] = Scope[Unit ! Rest, x] })#L, List[Unit ! Rest], Rest, A, A] {
        def ret(fins: List[Unit ! Rest], a: A) = fins.foldLeft(pure[Unit, Rest](()))((p, fin) => p.flatMap(_ => fin)).as(a)
        def answer[X](cell: Cell[List[Unit ! Rest]], op: Scope[Unit ! Rest, X]): X = op match {
          case Scope.Defer(fin, answer) => cell.set(fin :: cell.get); answer(())
        }
      })
}

object Resources {
  def apply(fx: Effects): Resources[fx.type] = new Resources[fx.type](fx)
}
