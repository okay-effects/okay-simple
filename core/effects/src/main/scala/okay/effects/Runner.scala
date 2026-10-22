package okay.effects

/**
 * AN IMPLEMENTATION THAT RUNS: the interface's words (`Effects`), and the run of a program of no effect. Code that
 * makes programs needs `Effects`; code that runs them, a `Runner` — the machine, `Free`, okay-core's `Core`.
 */
trait Runner extends Effects {
  def run[A](p: A ! Pure): A

  /** a program whose one effect is `Lift[G, *]` as a `G[A]`: each lifted `G[X]` bound in `G`, the rest run inside
   * `G`'s bind — a program of effects run in `IO`, a ZIO task, an `Option` */
  def lower[G[_], A](p: A ! ({ type L[x] = Std.Lift[G, x] })#L)(implicit G: Std.Lowering[G]): G[A] = {
    type L[x] = Std.Lift[G, x]
    run(handle[L, L, Pure, A, G[A]](p,
        new Handler[L, Pure, A, G[A]] {
          def ret(a: A) = pure[G[A], Pure](G.pure(a))
          def op[X](op: Std.Lift[G, X], k: X => G[A] ! Pure) = op match {
            case Std.Lift.Of(ga) => pure[G[A], Pure](G.flatMap(ga)(x => run(k(x))))
          }
        }))
  }
}
