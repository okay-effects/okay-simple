package okay.effects.jstream

import java.util.{Iterator => JIterator, Spliterator, Spliterators}
import java.util.stream.{StreamSupport, Stream => JStream}
import okay.effects.Runner
import okay.effects.stream.Streams

/**
 * JAVA'S STREAMS AND ITERATORS, and the stream module's streams, over an implementation that runs, `fx`, in the
 * implicit context: `JavaStreams()`.
 *
 *  - `fromJava(js)`: a Java stream as a stream of any row — made at the first pull, its elements pulled one at a time,
 *    as far as asked. A Java stream is used once; this one is not: each step remembered, so pulled twice, the same
 *    elements, and the Java stream made anew at each first pull of the whole.
 *  - `fromJavaIterator(it)`, `fromJavaIterable(xs)`: the same of an iterator (each step remembered), of an iterable.
 *  - `toJava(s)`, `toJavaIterator(s)`: a stream of no effect as a Java stream, an iterator — lazy, each element pulled
 *    by `fx`'s run when Java asks for it: endless streams too, `limit`ed on Java's side.
 */
class JavaStreams[I <: Runner with Singleton, S <: Streams[I]](val streams: S) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  import fx._
  import streams.{Emit, End, Pull, Stream}

  def fromJava[O, R[_]](js: => JStream[O]): Stream[O, R] =
    new Stream[O, R](() => delay(fromJavaIterator[O, R](js.iterator()).pull()))
  def fromJavaIterable[O, R[_]](xs: java.lang.Iterable[O]): Stream[O, R] =
    new Stream[O, R](() => delay(fromJavaIterator[O, R](xs.iterator()).pull()))

  /** a step of the iterator, taken once, when first pulled: the stream from it repeatable */
  def fromJavaIterator[O, R[_]](it: JIterator[O]): Stream[O, R] = new Stream[O, R]({
    lazy val step: Pull[O, R] =
      if (it.hasNext) { val o = it.next(); val rest = fromJavaIterator[O, R](it); new Emit[O, R](o, rest) }
      else new End[O, R]
    () => delay(pure[Pull[O, R], R](step))
  })

  /** the elements of `s`, pulled when the iterator is asked for the next */
  def toJavaIterator[O](s: Stream[O, Pure]): JIterator[O] = new JIterator[O] {
    private var current: Stream[O, Pure] = s
    private var ahead: Option[Option[O]] = None
    private def look(): Option[O] = ahead match {
      case Some(o) => o
      case None =>
        val o = run(current.pull()).fold[Option[O]](None) { (o, rest) => current = rest; Some(o) }
        ahead = Some(o)
        o
    }
    def hasNext: Boolean = look().isDefined
    def next(): O = look() match {
      case Some(o) => ahead = None; o
      case None => throw new NoSuchElementException("the stream has ended")
    }
  }

  /** `s` as a Java stream, sequential and ordered */
  def toJava[O](s: Stream[O, Pure]): JStream[O] =
    StreamSupport.stream(Spliterators.spliteratorUnknownSize(toJavaIterator(s), Spliterator.ORDERED), false)
}

object JavaStreams {
  def apply()(implicit fx: Runner): JavaStreams[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new JavaStreams[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
