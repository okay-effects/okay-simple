package okay.effects.tagless

import okay.effects.Runner

/**
 * TAGLESS RUN ON `fx`: `Tagless.on(fx)`, a `Runner` — `Tagless`'s words, its run `fx`'s. So code that runs programs
 * over the interface (the interface's suite, the examples) runs on tagless as on any implementation; its programs are
 * the values `Tagless` makes, of the same types.
 */
final class TaglessRunner(val fx: Runner) extends Runner:
  type ![+A, R[_]] = Program[A, R]
  type In[E[_], R[_]] = InRow[E, R]
  type Split[E[_], R[_], Rest[_]] = SplitRow[E, R, Rest]
  type Sub[R[_], S[_]] = SubRow[R, S]

  implicit inline def splitOne[E[_]]: Split[E, E, Pure] = Tagless.splitOne[E]
  implicit inline def splitLast[R[_], E[_]]: Split[E, R + E, R] = Tagless.splitLast[R, E]
  implicit inline def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Tagless.splitInit[E, R, G, Rest](s)
  implicit inline def subPure[S[_]]: Sub[Pure, S] = Tagless.subPure[S]
  implicit inline def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Tagless.subOne[E, S](e)
  implicit inline def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Tagless.subMore[R, E, S](r, e)

  inline def pure[A, R[_]](a: A): A ! R = Tagless.pure[A, R](a)
  inline def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Tagless.flatMap[A, B, R](p)(f)
  override inline def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = Tagless.map[A, B, R](p)(f)
  inline def delay[A, R[_]](p: => A ! R): A ! R = Tagless.delay[A, R](p)
  protected[effects] inline def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = Program.Performed[E, R, X](op, in)

  inline def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! (Rest + E) = Tagless.split[E, R, Rest, A](p)(s)
  inline def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = Tagless.embed[R, S, A](p)(s)
  inline def run[A](p: A ! Pure): A = Tagless.run[A](p)(using fx)

  inline def handleLast[E[_], R[_], A, B](p: A ! (R + E), h: Handler[E, R, A, B]): B ! R = Tagless.handleLast[E, R, A, B](p, h)
  // each shape the implementation's own, from its place
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Tagless.handle[E, R, Rest, A, B](p, h)(s)
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Tagless.handle[E, R, Rest, A, B](p, h)(s)
  override inline def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Tagless.handle[E, R, Rest, A, B](p, h)(s)
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest = Tagless.handle[E, S, R, Rest, A, B](p, s0, h)(s)
  override inline def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest = Tagless.handle[E, S, R, Rest, A, B](p, s0, h)(s)
