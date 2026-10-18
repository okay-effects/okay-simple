package okay

import scala.language.implicitConversions
import okay.effects.machine.Machine

/**
 * THE INTERFACE ON THE MACHINE, BY DEFAULT: `import okay.effects._` gives a program's types and words — `!`, `Has`,
 * `perform`, `handle` of each shape, `intercept`, `run` (`%`, `*`, `+` are the package's own names) — on the machine;
 * `import okay.effects.std._` its standard effects; `import okay.effects.delimited._` its delimited control changing
 * answer types. Each module's package does the same for the module: `import okay.effects.stream._`.
 */
package object effects {
  type ![+A, R[_]] = Machine.![A, R]
  type Pure[X] = Void[X]
  type In[E[_], R[_]] = Machine.In[E, R]
  type Split[E[_], R[_], Rest[_]] = Machine.Split[E, R, Rest]
  type Sub[R[_], S[_]] = Machine.Sub[R, S]
  type Has[E[_]] = Machine.Has[E]
  type Handler[E[_], R[_], A, B] = Machine.Handler[E, R, A, B]
  type TailHandler[E[_], R[_], A, B] = Machine.TailHandler[E, R, A, B]
  type AbortHandler[E[_], R[_], A, B] = Machine.AbortHandler[E, R, A, B]
  type StatefulHandler[E[_], S, R[_], A, B] = Machine.StatefulHandler[E, S, R, A, B]
  type StatefulTailHandler[E[_], S, R[_], A, B] = Machine.StatefulTailHandler[E, S, R, A, B]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = Machine.splitOne[E]
  implicit def splitLast[R[_], E[_]]: Split[E, ({ type L[x] = Sum[R, E, x] })#L, R] = Machine.splitLast[R, E]
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]):
      Split[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] = Machine.splitInit[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Machine.subPure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Machine.subOne[E, S](e)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[({ type L[x] = Sum[R, E, x] })#L, S] =
    Machine.subMore[R, E, S](r, e)
  /** a program's methods: `p.flatMap(f)`, `p.map(f)`, `p.into[S]` — named as the implementation's, so an import of an
   * implementation's words (`import Machine._`) shadows it, not two conversions */
  implicit def ProgramOps[A, R[_]](p: A ! R): Machine.ProgramOps[A, R] = new Machine.ProgramOps[A, R](p)

  def pure[A, R[_]](a: A): A ! R = Machine.pure[A, R](a)
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Machine.flatMap[A, B, R](p)(f)
  def map[A, B, R[_]](p: A ! R)(f: A => B): B ! R = Machine.map[A, B, R](p)(f)
  def delay[A, R[_]](p: => A ! R): A ! R = Machine.delay[A, R](p)
  def tailcall[A, R[_]](p: => A ! R): A ! R = Machine.tailcall[A, R](p)
  def tailRecM[A, B, R[_]](a: A)(f: A => Either[A, B] ! R): B ! R = Machine.tailRecM[A, B, R](a)(f)
  def perform[E[_]]: Perform[Machine.type, E] = Machine.perform[E]
  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! ({ type L[x] = Sum[Rest, E, x] })#L =
    Machine.split[E, R, Rest, A](p)(s)
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = Machine.embed[R, S, A](p)(s)

  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Machine.handle[E, R, Rest, A, B](p, h)(s)
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: TailHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Machine.handle[E, R, Rest, A, B](p, h)(s)
  def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: AbortHandler[E, Rest, A, B])(implicit s: Split[E, R, Rest]): B ! Rest =
    Machine.handle[E, R, Rest, A, B](p, h)(s)
  def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest = Machine.handle[E, S, R, Rest, A, B](p, s0, h)(s)
  def handle[E[_], S, R[_], Rest[_], A, B](p: A ! R, s0: S, h: StatefulHandler[E, S, Rest, A, B])(
      implicit s: Split[E, R, Rest]): B ! Rest = Machine.handle[E, S, R, Rest, A, B](p, s0, h)(s)
  def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: Handler[E, R, A, B]): B ! R =
    Machine.handleLast[E, R, A, B](p, h)
  def intercept[E[_], R[_], A, B](p: A ! R, h: Handler[E, R, A, B])(implicit in: In[E, R]): B ! R =
    Machine.intercept[E, R, A, B](p, h)(in)

  def run[A](p: A ! Pure): A = Machine.run[A](p)
  def lower[G[_], A](p: A ! ({ type L[x] = Std.Lift[G, x] })#L)(implicit G: Std.Lowering[G]): G[A] = Machine.lower[G, A](p)(G)

  /** the standard effects on the machine: `import okay.effects.std._` */
  val std: Machine.std.type = Machine.std
  /** delimited control changing answer types on the machine: `import okay.effects.delimited._` */
  val delimited: Machine.delimited.type = Machine.delimited
}
