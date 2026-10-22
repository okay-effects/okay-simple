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
class AsyncStreams[I <: Runner with Singleton, S <: Streams[I]](val streams: S)(implicit ec: ExecutionContext) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  private implicit val exact: fx.type = fx
  val asynchrony: Asynchrony[fx.type] = new Asynchrony[fx.type]
  import fx._
  import streams.{Pull, Stream}

  // `asynchrony.`'s words by name, not imported: in Scala 2 the package's own (its default, on the machine) would win
  /** the streams merged as their elements come */
  def mergeAsync[O](ss: List[Stream[O, Async]]): Stream[O, Async] =
    new Stream[O, Async](() =>
      racing[O](ss.zipWithIndex.map { case (s, i) => (i, asynchrony.toFuture(s.pull())) }).pull())

  /** the same, the streams given one by one: `mergeAsync(a, b, c)` */
  def mergeAsync[O](first: Stream[O, Async], rest: Stream[O, Async]*): Stream[O, Async] =
    mergeAsync[O](first :: rest.toList)

  /** the pulls on their way, each with its stream's number: the first to come given out, its stream's next pulled */
  private def racing[O](pending: List[(Int, Future[Pull[O, Async]])]): Stream[O, Async] = new Stream[O, Async](() =>
    if (pending.isEmpty) streams.empty[O, Async].pull()
    else asynchrony.fromFuture[(Int, Pull[O, Async]), Async](
      Future.firstCompletedOf(pending.map { case (i, f) => f.map(p => (i, p)) })).flatMap { case (i, pull) =>
      val others = pending.filterNot(_._1 == i)
      pull.fold(racing[O](others).pull())((o, rest) =>
        pure[Pull[O, Async], Async](new streams.Emit[O, Async](o,
            racing[O](others :+ ((i, asynchrony.toFuture(rest.pull())))))))
    })
}

object AsyncStreams {
  def apply()(implicit fx: Runner, ec: ExecutionContext): AsyncStreams[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new AsyncStreams[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
