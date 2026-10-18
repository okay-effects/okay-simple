package okay.effects.kyointerop

import _root_.kyo.{<, Chunk, Emit, Frame, Kyo, Maybe, Tag}
import _root_.okay.effects.{Runner, Std}  // from the root, and no wildcard import of `fx`: the package object of defaults is this class
import _root_.okay.effects.stream.Streams

/**
 * KYO, and the stream module's streams, over an implementation that runs, in the implicit context: `KyoEffects()`
 * (Scala 3: kyo is Scala 3's). A kyo computation `A < S` — of kyo's pending effects `S` — is a standard `Lift`:
 *
 *  - `fromKyo(v)`: a kyo computation performed inside a program, its value the answer;
 *  - `toKyo(p)`: a program whose one effect is `Kyos[S]` as an `A < S`, bound by kyo's `map` — kyo's handlers
 *    (`Var.run`, `Abort.run`, `Env.run`, ...) then run it;
 *  - `toKyoStream(s)`: a stream of `Kyos[S]` a kyo `Stream[O, S]`, each pull a computation, as far as kyo asks;
 *  - `fromKyoStream(ks)`: a kyo stream a stream here — run to its end at the first pull, its elements then given out
 *    (kyo has no public pull of one chunk).
 */
class KyoEffects[I <: Runner, S0 <: Streams[I]](val streams: S0) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  import streams.{Pull, Stream}

  /** the effect of kyo's computations of `S`: the standard `Lift` of them */
  type Kyos[S] = { type L[X] = Std.Lift[({ type K[A] = A < S })#K, X] }

  implicit def lowering[S]: Std.Lowering[({ type K[A] = A < S })#K] = new Std.Lowering[({ type K[A] = A < S })#K] {
    def pure[A](a: A): A < S = Kyo.lift[A, S](a)
    def flatMap[A, B](ga: A < S)(f: A => B < S): B < S = ga.map(f)
  }

  def fromKyo[X, S, R[_]](v: X < S)(implicit in: fx.In[Kyos[S]#L, R]): fx.![X, R] = fx.std.lift[({ type K[A] = A < S })#K, X, R](v)
  def toKyo[A, S](p: fx.![A, Kyos[S]#L]): A < S = fx.lower[({ type K[A] = A < S })#K, A](p)(using lowering[S])

  def toKyoStream[O, S](s: Stream[O, Kyos[S]#L])(implicit tag: Tag[Emit[Chunk[O]]], frame: Frame): _root_.kyo.Stream[O, S] =
    _root_.kyo.Stream.unfold[Stream[O, Kyos[S]#L], O, S](s)(cur =>
      toKyo[Pull[O, Kyos[S]#L], S](cur.pull()).map(_.fold[Maybe[(O, Stream[O, Kyos[S]#L])]](Maybe.empty)((o, rest) => Maybe((o, rest)))))

  def fromKyoStream[O, S](ks: => _root_.kyo.Stream[O, S])(implicit tag: Tag[Emit[Chunk[O]]], frame: Frame): Stream[O, Kyos[S]#L] =
    new Stream[O, Kyos[S]#L](() =>
      fx.flatMap[Chunk[O], Pull[O, Kyos[S]#L], Kyos[S]#L](fromKyo[Chunk[O], S, Kyos[S]#L](ks.run))(chunk => streams.emits[O, Kyos[S]#L](chunk.toList).pull()))
}

object KyoEffects {
  def apply()(implicit fx: Runner): KyoEffects[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new KyoEffects[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
