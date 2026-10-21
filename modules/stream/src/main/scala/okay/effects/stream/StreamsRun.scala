package okay.effects.stream

import okay.effects.Runner

/**
 * A STREAM OF NO EFFECT AS A SCALA COLLECTION, run by the implementation that runs: `toLazyList(s)`, `toIterator(s)` —
 * lazy, each element pulled by the run when asked, so an endless stream is taken as far as asked. Over the streams it
 * is given; in `import okay.effects.stream._`, the default's.
 */
class StreamsRun[I <: Runner with Singleton, S <: Streams[I]](val streams: S) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  // `streams.Stream` by name, not imported: in Scala 2 this package's own (its default, on the machine) would win

  def toLazyList[O](s: streams.Stream[O, fx.Pure]): LazyList[O] =
    fx.run(s.pull()).fold[LazyList[O]](LazyList.empty)((o, rest) => o #:: toLazyList[O](rest))

  def toIterator[O](s: streams.Stream[O, fx.Pure]): Iterator[O] = new Iterator[O] {
    private var current: streams.Stream[O, fx.Pure] = s
    private var ahead: Option[Option[O]] = None
    private def look(): Option[O] = ahead match {
      case Some(o) => o
      case None =>
        val o = fx.run(current.pull()).fold[Option[O]](None) { (o, rest) => current = rest; Some(o) }
        ahead = Some(o)
        o
    }
    def hasNext: Boolean = look().isDefined
    def next(): O = look() match {
      case Some(o) => ahead = None; o
      case None => Iterator.empty[O].next()
    }
  }
}

object StreamsRun {
  def apply()(implicit fx: Runner): StreamsRun[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new StreamsRun[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
