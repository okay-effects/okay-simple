package okay.effects.monixinterop

import _root_.monix.eval.Task
import _root_.monix.execution.Scheduler
import _root_.monix.reactive.Observable
import _root_.monix.tail.Iterant
import okay.effects.{Runner, Std}
import okay.effects.stream.Streams

/**
 * MONIX: its `Task` and its `Observable`, and the stream module's streams, over an implementation that runs, `fx`,
 * in the implicit context: `Monix()`. Monix 3 is of cats-effect 2: not in one application with cats-effect 3.
 *
 *  - `fromTask(task)`, `toTask(p)`: a task performed inside a program, the standard `Lift`; a program of `Tasks` a task;
 *  - `toObservable(s)`: a stream of `Tasks` an `Observable`, each pull a task; `toObservablePure(s)`, of no effect;
 *  - `fromObservable(obs)`: an `Observable` a stream of `Tasks`, pulled as far as asked — through monix's `Iterant`,
 *    its back-pressure ours; subscribed at the first pull, released at its end or its failure.
 */
class Monix[I <: Runner with Singleton, S <: Streams[I]](val streams: S) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  import fx._
  import streams.{Emit, End, Pull, Stream}

  /** the effect of monix's tasks: the standard `Lift` of `Task` */
  type Tasks[X] = Std.Lift[Task, X]

  implicit val taskLowering: Std.Lowering[Task] = new Std.Lowering[Task] {
    def pure[A](a: A): Task[A] = Task.now(a)
    def flatMap[A, B](ga: Task[A])(f: A => Task[B]): Task[B] = ga.flatMap(a => Task.defer(f(a)))
  }

  def fromTask[X, R[_]](task: Task[X])(implicit in: In[Tasks, R]): X ! R = std.lift[Task, X, R](task)
  def toTask[A](p: A ! Tasks): Task[A] = fx.lower[Task, A](p)

  def toObservable[O](s: Stream[O, Tasks]): Observable[O] =
    Observable.unfoldEval[Stream[O, Tasks], O](s)(cur =>
      toTask(cur.pull()).map(_.fold[Option[(O, Stream[O, Tasks])]](None)((o, rest) => Some((o, rest)))))

  def toObservablePure[O](s: Stream[O, Pure]): Observable[O] =
    Observable.unfold[Stream[O, Pure], O](s)(cur => run(cur.pull()).fold[Option[(O, Stream[O, Pure])]](None)((o, rest) => Some((o, rest))))

  def fromObservable[O](obs: Observable[O])(implicit scheduler: Scheduler): Stream[O, Tasks] =
    new Stream[O, Tasks](() =>
      fromTask[(Task[Either[Option[Throwable], O]], Task[Unit]), Tasks](
        Iterant.fromReactivePublisher[Task, O](obs.toReactivePublisher).consume.allocated.map { case (c, release) => (c.pull, release) })
        .flatMap { case (next, release) => pulling[O](next, release).pull() })

  /** the next element, the consumer's — at the end or a failure, released */
  private def pulling[O](next: Task[Either[Option[Throwable], O]], release: Task[Unit]): Stream[O, Tasks] =
    new Stream[O, Tasks](() => fromTask[Option[O], Tasks](next.flatMap {
      case Left(None) => release.map(_ => None)
      case Left(Some(e)) => release.flatMap(_ => Task.raiseError(e))
      case Right(o) => Task.now(Some(o))
    }).flatMap {
      case None => pure[Pull[O, Tasks], Tasks](new End[O, Tasks])
      case Some(o) => pure[Pull[O, Tasks], Tasks](new Emit[O, Tasks](o, pulling[O](next, release)))
    })
}

object Monix {
  def apply()(implicit fx: Runner): Monix[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new Monix[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
