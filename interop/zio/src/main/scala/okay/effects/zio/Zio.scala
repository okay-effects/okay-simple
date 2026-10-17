package okay.effects.zio

import okay.effects.{Runner, Std}
import zio.{Task, ZIO}

/**
 * ZIO OVER THE EFFECTS, on any implementation `fx` (one source for both compilers): a task is a standard `Lift` —
 *
 *  - `fromZIO(task)`: a ZIO task performed inside a program, `std.lift`;
 *  - `toZIO(p)`: a program whose one effect is `Lift[Task, *]` as a `Task[A]`, `lower` by ZIO's bind — the rest
 *    suspended in it, so a throw in it is the task's failure.
 *
 * `val z = ZioEffects(Machine); import z._`
 */
class ZioEffects[F <: Runner](val fx: F) {
  import fx._

  /** the effect of ZIO's tasks: the standard `Lift` of `Task` */
  type Tasks[X] = Std.Lift[Task, X]

  implicit val taskLowering: Std.Lowering[Task] = new Std.Lowering[Task] {
    def pure[A](a: A): Task[A] = ZIO.succeed(a)
    def flatMap[A, B](ga: Task[A])(f: A => Task[B]): Task[B] = ga.flatMap(a => ZIO.suspend(f(a)))
  }

  def fromZIO[X, R[_]](task: Task[X])(implicit in: In[Tasks, R]): X ! R = std.lift[Task, X, R](task)
  def toZIO[A](p: A ! Tasks): Task[A] = fx.lower[Task, A](p)
}

object ZioEffects {
  def apply(fx: Runner): ZioEffects[fx.type] = new ZioEffects[fx.type](fx)
}
