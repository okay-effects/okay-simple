package okay.effects.stream

import scala.collection.immutable.Queue
import okay.effects.{Effects, Std}

/**
 * STREAMS over any implementation `fx`: a `Stream[O, R]` gives elements `O` one at a time, each pull a program of the
 * row `R` — so its elements may be made by effects (`eval`, `evalMap`), and it is LAZY: nothing is pulled until a
 * sink asks (`toList`, `fold`, `foreach`, `drain`), an endless stream taken as far as it is asked. A stream is a value:
 * pulled twice, it runs twice. With the standard generators, `Yield`: `fromGenerator(p)`, a program that `emit`s, as a
 * stream; `s.emitAll`, a stream as a program that `emit`s.
 */
class Streams[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation
   * to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  /** one pull of a stream: its end, or an element and the stream after it */
  sealed abstract class Pull[O, R[_]] {
    def fold[Z](end: => Z)(emit: (O, Stream[O, R]) => Z): Z
  }
  final class End[O, R[_]] extends Pull[O, R] {
    def fold[Z](end: => Z)(emit: (O, Stream[O, R]) => Z): Z = end
  }
  final class Emit[O, R[_]](o: O, rest: => Stream[O, R]) extends Pull[O, R] {
    def fold[Z](end: => Z)(emit: (O, Stream[O, R]) => Z): Z = emit(o, rest)
  }
  private def ended[O, R[_]]: Pull[O, R] ! R = pure[Pull[O, R], R](new End[O, R])
  private def emitted[O, R[_]](o: O, rest: => Stream[O, R]): Pull[O, R] ! R =
    pure[Pull[O, R], R](new Emit[O, R](o, rest))

  /** A STREAM of `O` over `R`: `pull`, the program giving its first element and the rest, or its end */
  final class Stream[O, R[_]](val pull: () => Pull[O, R] ! R) {
    private def step[O2](onEnd: => Pull[O2, R] ! R)(onEmit: (O, Stream[O, R]) => Pull[O2, R] ! R): Stream[O2, R] =
      new Stream[O2, R](() => pull().flatMap(_.fold(onEnd)(onEmit)))

    def map[O2](f: O => O2): Stream[O2, R] = step[O2](ended)((o, r) => emitted(f(o), r.map(f)))
    /** each element through `f`, a program of the row */
    def evalMap[O2](f: O => O2 ! R): Stream[O2, R] =
      step[O2](ended)((o, r) => f(o).flatMap(o2 => emitted(o2, r.evalMap(f))))
    def filter(p: O => Boolean): Stream[O, R] =
      step[O](ended)((o, r) => if (p(o)) emitted(o, r.filter(p)) else r.filter(p).pull())
    def take(n: Int): Stream[O, R] = if (n <= 0) empty[O, R] else step[O](ended)((o, r) => emitted(o, r.take(n - 1)))
    def drop(n: Int): Stream[O, R] = if (n <= 0) this else step[O](ended)((_, r) => r.drop(n - 1).pull())
    def takeWhile(p: O => Boolean): Stream[O, R] =
      step[O](ended)((o, r) => if (p(o)) emitted(o, r.takeWhile(p)) else ended)
    def dropWhile(p: O => Boolean): Stream[O, R] =
      step[O](ended)((o, r) => if (p(o)) r.dropWhile(p).pull() else emitted(o, r))
    /** `z`, then each running result of `f` */
    def scan[Z](z: Z)(f: (Z, O) => Z): Stream[Z, R] =
      new Stream[Z, R](() => emitted(z, step[Z](ended)((o, r) => r.scan(f(z, o))(f).pull())))
    def zipWithIndex: Stream[(O, Long), R] = zipFrom(0L)
    private def zipFrom(i: Long): Stream[(O, Long), R] =
      step[(O, Long)](ended)((o, r) => emitted((o, i), r.zipFrom(i + 1)))
    /** this stream, then `that` */
    def ++(that: => Stream[O, R]): Stream[O, R] = step[O](that.pull())((o, r) => emitted(o, r ++ that))
    def flatMap[O2](f: O => Stream[O2, R]): Stream[O2, R] = step[O2](ended)((o, r) => (f(o) ++ r.flatMap(f)).pull())
    /** pairs, one of each, to the end of the shorter */
    def zip[O2](that: Stream[O2, R]): Stream[(O, O2), R] =
      step[(O, O2)](ended)((a, r1) =>
        that.pull().flatMap(_.fold(ended[(O, O2), R])((b, r2) => emitted((a, b), r1.zip(r2)))))
    /** one of this, one of that, in turn; the rest of the longer at the end */
    def interleave(that: Stream[O, R]): Stream[O, R] = step[O](that.pull())((o, r) => emitted(o, that.interleave(r)))

    def fold[Z](z: Z)(f: (Z, O) => Z): Z ! R = pull().flatMap(_.fold(pure[Z, R](z))((o, r) => r.fold(f(z, o))(f)))
    def toList: List[O] ! R = fold[List[O]](Nil)((acc, o) => o :: acc).map(_.reverse)
    def foreach(f: O => Unit ! R): Unit ! R =
      pull().flatMap(_.fold(pure[Unit, R](()))((o, r) => f(o).flatMap(_ => r.foreach(f))))
    def drain: Unit ! R = foreach(_ => pure[Unit, R](()))
    def headOption: Option[O] ! R = pull().map(_.fold[Option[O]](None)((o, _) => Some(o)))
    /** this stream as a program that `emit`s each element, in a row `S` of more effects */
    def emitAll[S[_]](implicit y: In[({ type L[x] = Yield[O, x] })#L, S], sub: Sub[R, S]): Unit ! S =
      pull().into[S].flatMap(_.fold(pure[Unit, S](()))((o, r) => emit[O, S](o).flatMap(_ => r.emitAll[S])))
  }

  def empty[O, R[_]]: Stream[O, R] = new Stream[O, R](() => ended)
  def emits[O, R[_]](xs: List[O]): Stream[O, R] =
    new Stream[O, R](() => xs match { case o :: t => emitted(o, emits[O, R](t)); case Nil => ended })
  /** `from` up to `until`, not including it */
  def range[R[_]](from: Int, until: Int): Stream[Int, R] =
    new Stream[Int, R](() => if (from < until) emitted(from, range[R](from + 1, until)) else ended)
  /** `x`, `f(x)`, `f(f(x))`, ... — endless */
  def iterate[O, R[_]](x: O)(f: O => O): Stream[O, R] = new Stream[O, R](() => emitted(x, iterate[O, R](f(x))(f)))
  def unfold[S, O, R[_]](s: S)(f: S => Option[(O, S)]): Stream[O, R] =
    new Stream[O, R](() => f(s) match { case Some((o, s1)) => emitted(o, unfold[S, O, R](s1)(f)); case None => ended })
  /** one element, made by `p` */
  def eval[O, R[_]](p: O ! R): Stream[O, R] = new Stream[O, R](() => p.flatMap(o => emitted(o, empty[O, R])))
  /** an element made by `p` each pull — endless */
  def repeatEval[O, R[_]](p: O ! R): Stream[O, R] =
    new Stream[O, R](() => p.flatMap(o => emitted(o, repeatEval[O, R](p))))

  /** THE STREAMS MERGED, IN TURN: one element of each in turn, a stream that has ended left out — no channel, no
   * buffer: each pull the next stream's own */
  def merge[O, R[_]](ss: List[Stream[O, R]]): Stream[O, R] = mergeQueue[O, R](Queue.empty[Stream[O, R]] ++ ss)
  /** the same, the streams given one by one: `merge(a, b, c)` */
  def merge[O, R[_]](first: Stream[O, R], rest: Stream[O, R]*): Stream[O, R] = merge[O, R](first :: rest.toList)
  private def mergeQueue[O, R[_]](q: Queue[Stream[O, R]]): Stream[O, R] = new Stream[O, R](() =>
    q.dequeueOption match {
      case None => ended
      case Some((s, rest)) =>
        s.pull().flatMap(_.fold(mergeQueue[O, R](rest).pull())((o, r) => emitted(o, mergeQueue[O, R](rest.enqueue(r)))))
    })

  /** A SCALA ITERATOR as a stream of any row: made at the first pull, its elements pulled one at a time, as far as
   * asked; each step remembered, so pulled twice, the same elements */
  def fromIterator[O, R[_]](it: => Iterator[O]): Stream[O, R] = new Stream[O, R](() => delay(stepping[O, R](it).pull()))
  /** an iterable, a `List`, a `Vector`, a `Range`: its iterator's */
  def fromIterable[O, R[_]](xs: Iterable[O]): Stream[O, R] = fromIterator[O, R](xs.iterator)
  /** a lazy list, endless or not: its elements as it makes them */
  def fromLazyList[O, R[_]](ll: LazyList[O]): Stream[O, R] = new Stream[O, R](() =>
    delay(
      if (ll.isEmpty) ended[O, R] else emitted[O, R](ll.head, fromLazyList[O, R](ll.tail))))
  /** a step of the iterator, taken once, when first pulled */
  private def stepping[O, R[_]](it: Iterator[O]): Stream[O, R] = new Stream[O, R]({
    lazy val step: Pull[O, R] =
      if (it.hasNext) { val o = it.next(); val rest = stepping[O, R](it); new Emit[O, R](o, rest) }
      else new End[O, R]
    () => pure[Pull[O, R], R](step)
  })

  /** a generator — a program `emit`ting `O` — as a stream of the row outside its `Yield`: pulled as far as asked, the
   * generator run that far, its rest the stream's */
  def fromGenerator[O]: FromGenerator[O] = new FromGenerator[O]
  final class FromGenerator[O] {
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): Stream[O, Rest] =
      new Stream[O, Rest](() =>
        handle[({ type L[x] = Yield[O, x] })#L, R, Rest, A, Pull[O, Rest]](p,
          new Handler[({ type L[x] = Yield[O, x] })#L, Rest, A, Pull[O, Rest]] {
            def ret(a: A) = ended[O, Rest]
            def op[X](op: Yield[O, X], k: X => Pull[O, Rest] ! Rest) = op match {
              case Std.Yield.Emit(o, answer) => emitted[O, Rest](o, new Stream[O, Rest](() => k(answer(()))))
            }
          }))
  }
}

object Streams {
  def apply()(implicit fx: Effects): Streams[fx.type] = {
    implicit val exact: fx.type = fx
    new Streams[fx.type]
  }
}
