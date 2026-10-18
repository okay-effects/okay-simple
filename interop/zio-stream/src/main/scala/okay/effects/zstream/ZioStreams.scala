package okay.effects.zstream

import okay.effects.Runner
import okay.effects.stream.Streams
import okay.effects.ziointerop.ZioEffects
import zio.{Chunk, Exit, Scope, ZIO}
import zio.stream.ZStream

/**
 * ZIO'S STREAMS, and the stream module's streams, over an implementation that runs, `fx`, in the implicit context:
 * `ZioStreams()`. A stream of `Tasks` — its pulls performing ZIO's tasks — is a `ZStream` and back:
 *
 *  - `toZStream(s)`: each pull of `s` a task, `toZIO`; the stream's end ZIO's;
 *  - `toZStreamPure(s)`: a stream of no effect, each pull run by `fx`;
 *  - `fromZStream(zs)`: a `ZStream` as a stream of `Tasks`, pulled a chunk at a time, as far as asked. Made at the
 *    first pull: its scope opened then, closed at its end or its failure — so a stream not pulled to its end keeps
 *    the `ZStream`'s scope open; take what you need from it with `ZStream`'s `take` on ZIO's side when that matters.
 */
class ZioStreams[I <: Runner, S <: Streams[I]](val streams: S) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  private implicit val exact: fx.type = fx
  val zioEffects: ZioEffects[fx.type] = new ZioEffects[fx.type]
  import fx._
  import zioEffects.{Tasks, fromZIOTask, toZIO}
  import streams.{End, Pull, Stream}

  def toZStream[O](s: Stream[O, Tasks]): ZStream[Any, Throwable, O] =
    ZStream.unfoldZIO(s)(cur => toZIO(cur.pull()).map(_.fold[Option[(O, Stream[O, Tasks])]](None)((o, rest) => Some((o, rest)))))

  def toZStreamPure[O](s: Stream[O, Pure]): ZStream[Any, Nothing, O] =
    ZStream.unfold(s)(cur => run(cur.pull()).fold[Option[(O, Stream[O, Pure])]](None)((o, rest) => Some((o, rest))))

  def fromZStream[O](zs: => ZStream[Any, Throwable, O]): Stream[O, Tasks] = new Stream[O, Tasks](() =>
    fromZIOTask[(Scope.Closeable, ZIO[Any, Option[Throwable], Chunk[O]]), Tasks](
      Scope.make.flatMap(scope => scope.extend[Any](zs.toPull).map(pull => (scope, pull))))
      .flatMap { case (scope, pull) => pulling(scope, pull).pull() })

  /** the next chunk pulled, its elements given out one at a time, then the next */
  private def pulling[O](scope: Scope.Closeable, pull: ZIO[Any, Option[Throwable], Chunk[O]]): Stream[O, Tasks] =
    new Stream[O, Tasks](() => fromZIOTask[Option[Chunk[O]], Tasks](pull.foldZIO(
      {
        case None => scope.close(Exit.unit).as(None)
        case Some(e) => scope.close(Exit.fail(e)) *> ZIO.fail(e)
      },
      chunk => ZIO.succeed(Some(chunk)))).flatMap {
      case None => pure[Pull[O, Tasks], Tasks](new End[O, Tasks])
      case Some(chunk) => (streams.emits[O, Tasks](chunk.toList) ++ pulling(scope, pull)).pull()
    })
}

object ZioStreams {
  def apply()(implicit fx: Runner): ZioStreams[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new ZioStreams[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
