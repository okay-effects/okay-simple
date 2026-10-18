package okay.effects.fs2interop

import _root_.cats.Monad
import okay.effects.{Runner, Std}
import okay.effects.catsinterop.Cats
import okay.effects.stream.Streams

/**
 * FS2'S STREAMS, and the stream module's streams, over an implementation that runs, `fx`, in the implicit context:
 * `Fs2Streams()`. A stream of `Lifted[F]` — its pulls performing `F`'s, cats-effect's `IO` or any `F` with a cats
 * `Monad` — is an `fs2.Stream[F, O]`, and back:
 *
 *  - `toFs2(s)`: each pull of `s` an `F`, lowered by cats' monad; the stream's end fs2's;
 *  - `toFs2Pure(s)`: a stream of no effect, each pull run by `fx`;
 *  - `fromFs2(fs)`: an fs2 stream a stream of `Lifted[F]`, a chunk at a time, as far as asked — each step compiled on
 *    its own (`uncons`), so a resource the fs2 stream holds across its elements is released at each step's end: for
 *    such a stream, compile it on fs2's side and lift what it gives.
 */
class Fs2Streams[I <: Runner, S <: Streams[I]](val streams: S) {
  /** the implementation: the streams' own */
  val fx: streams.fx.type = streams.fx
  private implicit val exact: fx.type = fx
  val catsInterop: Cats[fx.type] = new Cats[fx.type]
  import fx._
  import streams.{End, Pull, Stream}

  /** the effect of `F`'s programs: the standard `Lift` of `F` */
  type Lifted[F[_]] = { type L[X] = Std.Lift[F, X] }

  def toFs2[F[_], O](s: Stream[O, Lifted[F]#L])(implicit F: Monad[F]): _root_.fs2.Stream[F, O] = {
    implicit val lowering: Std.Lowering[F] = catsInterop.lowering[F]
    _root_.fs2.Stream.unfoldEval[F, Stream[O, Lifted[F]#L], O](s)(cur =>
      F.map(fx.lower[F, Pull[O, Lifted[F]#L]](cur.pull()))(
        _.fold[Option[(O, Stream[O, Lifted[F]#L])]](None)((o, rest) => Some((o, rest)))))
  }

  def toFs2Pure[O](s: Stream[O, Pure]): _root_.fs2.Stream[_root_.fs2.Pure, O] =
    _root_.fs2.Stream.unfold[_root_.fs2.Pure, Stream[O, Pure], O](s)(cur =>
      run(cur.pull()).fold[Option[(O, Stream[O, Pure])]](None)((o, rest) => Some((o, rest))))

  def fromFs2[F[_], O](fs: _root_.fs2.Stream[F, O])(implicit compiler: _root_.fs2.Compiler[F, F], F: Monad[F]): Stream[O, Lifted[F]#L] = {
    type R[X] = Lifted[F]#L[X]
    type Step = Option[(_root_.fs2.Chunk[O], _root_.fs2.Stream[F, O])]
    val step: F[Step] = F.map(fs.pull.uncons.flatMap(o => _root_.fs2.Pull.output1(o)).stream.compile.last)(_.flatten)
    new Stream[O, R](() => std.lift[F, Step, R](step).flatMap {
      case None => pure[Pull[O, R], R](new End[O, R])
      case Some((chunk, rest)) => (streams.emits[O, R](chunk.toList) ++ fromFs2[F, O](rest)).pull()
    })
  }
}

object Fs2Streams {
  def apply()(implicit fx: Runner): Fs2Streams[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new Fs2Streams[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
