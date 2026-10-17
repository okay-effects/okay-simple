package okay.effects.async

import scala.concurrent.{ExecutionContext, Future}
import okay.effects.Runner
import okay.effects.stream.Streams

/**
 * ASYNCHRONOUS STREAMS over any implementation that runs, `fx`: streams of the row `Async` — each pull may wait —
 * and their MERGE as they come: each stream's next element pulled at once, all at the same time; the merged stream
 * gives whichever is ready first, and then pulls the next of that stream. A stream that ends is left out. Pulled a
 * step ahead: while the merged stream's element is used, each stream's next is on its way.
 */
class AsyncStreams[F <: Runner](val fx: F)(implicit ec: ExecutionContext) {
  val streams: Streams[fx.type] = new Streams[fx.type](fx)
  val asynchrony: Asynchrony[fx.type] = new Asynchrony[fx.type](fx)
  import fx._
  import streams.{Pull, Stream}
  import asynchrony.{fromFuture, toFuture}

  /** the streams merged as their elements come */
  def mergeAsync[O](ss: List[Stream[O, Async]]): Stream[O, Async] =
    new Stream[O, Async](() => racing[O](ss.zipWithIndex.map { case (s, i) => (i, toFuture(s.pull())) }).pull())

  /** the pulls on their way, each with its stream's number: the first to come given out, its stream's next pulled */
  private def racing[O](pending: List[(Int, Future[Pull[O, Async]])]): Stream[O, Async] = new Stream[O, Async](() =>
    if (pending.isEmpty) streams.empty[O, Async].pull()
    else fromFuture[(Int, Pull[O, Async]), Async](
      Future.firstCompletedOf(pending.map { case (i, f) => f.map(p => (i, p)) })).flatMap { case (i, pull) =>
        val others = pending.filterNot(_._1 == i)
        pull.fold(racing[O](others).pull())((o, rest) =>
          pure[Pull[O, Async], Async](new streams.Emit[O, Async](o, racing[O](others :+ ((i, toFuture(rest.pull())))))))
      })
}

object AsyncStreams {
  def apply(fx: Runner)(implicit ec: ExecutionContext): AsyncStreams[fx.type] = new AsyncStreams[fx.type](fx)
}
