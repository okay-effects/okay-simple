package okay.effects.ziointerop

import okay.effects.{Runner, Std, Sum}
import okay.effects.async.{Async, Asynchrony}
import zio.{Exit, Runtime, Task, Unsafe, ZEnvironment, ZIO}

/**
 * ZIO OVER THE EFFECTS, on any implementation `fx` (one source for both compilers): a task is a standard `Lift` —
 *
 *  - `fromZIO(task)`: a ZIO task performed inside a program, `std.lift`;
 *  - `toZIO(p)`: a program whose one effect is `Lift[Task, *]` as a `Task[A]`, `lower` by ZIO's bind — the rest
 *    suspended in it, so a throw in it is the task's failure.
 *
 * TYPED, each of ZIO's channels one of the effects: a `ZIO[Env, Err, A]` in a row `ZioRow[Env, Err]` —
 *
 *  - its environment `Env` the standard `Reader % ZEnvironment[Env]`, asked when it runs;
 *  - its typed error `Err` the standard `Error % Err`, raised;
 *  - its run the async module's `Async`: forked on ZIO's runtime, its value or its defect the callback's.
 *
 * `fromZIOTyped(z)` and `toZIOTyped(p)` (the reader ZIO's environment, a raise ZIO's failure, a throw a defect). The
 * row accumulates as ZIO's types do: ZIO is contravariant in `Env`, covariant in `Err`, so a `ZIO[Db, DbErr, A]` is a
 * `ZIO[Db & Log, AppErr, A]` — one row `ZioRow[Db & Log, AppErr]` for every ZIO of a program.
 *
 * `import okay.effects.ziointerop._`, the machine's; on another implementation `ZioEffects()`.
 */
class ZioEffects[F <: Runner](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation to the code it is imported into */
  val fx: F = implementation
  import fx._

  /** the effect of ZIO's tasks: the standard `Lift` of `Task` */
  type Tasks[X] = Std.Lift[Task, X]

  implicit val taskLowering: Std.Lowering[Task] = new Std.Lowering[Task] {
    def pure[A](a: A): Task[A] = ZIO.succeed(a)
    def flatMap[A, B](ga: Task[A])(f: A => Task[B]): Task[B] = ga.flatMap(a => ZIO.suspend(f(a)))
  }

  def fromZIO[X, R[_]](task: Task[X])(implicit in: In[Tasks, R]): X ! R = std.lift[Task, X, R](task)
  def toZIO[A](p: A ! Tasks): Task[A] = fx.lower[Task, A](p)

  private implicit val exact: fx.type = fx
  private implicit val context: scala.concurrent.ExecutionContext = Asynchrony.defaultContext
  /** asynchrony on this implementation: what runs a `ZioRow`'s `Async` */
  val asynchrony: Asynchrony[fx.type] = new Asynchrony[fx.type]

  /** the row of a ZIO of environment `Env` and error `Err`: `Async + Error % Err + Reader % ZEnvironment[Env]` */
  type ZioRow[Env, Err] = { type L[X] = Sum[({ type M[Y] = Sum[Async, ({ type E[Z] = Std.Error[Err, Z] })#E, Y] })#M,
    ({ type N[Y] = Std.Reader[ZEnvironment[Env], Y] })#N, X] }

  /** a ZIO run on `runtime` as an `Async`: its exit the callback's — its value, or a failure or defect as a throwable;
   * the wait cancelled, its fiber interrupted */
  def fromTaskAsync[A, R[_]](z: ZIO[Any, Nothing, A], runtime: Runtime[Any] = Runtime.default)(implicit as: In[Async, R]): A ! R =
    asynchrony.cancellable[A, R](cb => Unsafe.unsafe { implicit u =>
      val fiber = runtime.unsafe.fork(z)
      fiber.unsafe.addObserver {
        case Exit.Success(a) => cb(Right(a))
        case Exit.Failure(cause) => cb(Left(cause.squashTrace))
      }
      () => { runtime.unsafe.fork(fiber.interrupt); () }
    })

  /** a ZIO in the effects: its environment asked, its typed error raised, its run an `Async` */
  def fromZIOTyped[Env, Err, A, R[_]](z: ZIO[Env, Err, A], runtime: Runtime[Any] = Runtime.default)(
      implicit env: In[({ type N[Y] = Std.Reader[ZEnvironment[Env], Y] })#N, R], err: In[({ type E[Z] = Std.Error[Err, Z] })#E, R],
      as: In[Async, R]): A ! R =
    std.ask[ZEnvironment[Env], R].flatMap(e => fromTaskAsync[Either[Err, A], R](z.provideEnvironment(e).either, runtime)).flatMap {
      case Right(a) => pure[A, R](a)
      case Left(e) => std.raise[Err, R, A](e)
    }

  /** the program as a ZIO: its reader ZIO's environment, its raise ZIO's failure, a throw out of it a defect; the ZIO
   * interrupted, the program cancelled (the ZIOs it waits on interrupted in turn) */
  def toZIOTyped[Env, Err, A](p: A ! ZioRow[Env, Err]#L): ZIO[Env, Err, A] =
    ZIO.environmentWithZIO[Env] { (env: ZEnvironment[Env]) =>
      ZIO.asyncInterrupt[Any, Err, A] { done =>
        val running = asynchrony.start(std.attempt[Err](std.reader[ZEnvironment[Env]](env)(p)))
        running.future.onComplete {
          case scala.util.Success(Right(a)) => done(ZIO.succeed(a))
          case scala.util.Success(Left(e)) => done(ZIO.fail(e))
          case scala.util.Failure(t) => done(ZIO.die(t))
        }
        Left(ZIO.succeed(running.cancel()))
      }
    }
}

object ZioEffects {
  def apply()(implicit fx: Runner): ZioEffects[fx.type] = {
    implicit val exact: fx.type = fx
    new ZioEffects[fx.type]
  }
}
