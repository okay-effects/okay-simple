package okay.one.simple

import okay.one.{Cont, !}
import okay.one.handler.{Answering, Deep, Stateful, Translate}
import okay.one.machine.Machine
import okay.one.row.{+:, In, Removed, Row}
import okay.simple.{Ops, Simple}

/**
 * okay-simple's interface ON OKAY-CORE'S MACHINE: a program is okay-core's own `Cont`, nothing wrapped; okay-simple's
 * places are okay-core's paths — the last effect of an okay-simple row is the head of okay-core's — and `Split` is
 * okay-core's `Removed`, made of the place: the head, or one further in. A handler is one of okay-core's forms: a
 * general one a `Deep`, one answering in place an `Answering`, a stateful one a `Stateful` (the machine's register).
 *
 * In the package `okay.one` for the kernel's nodes its own API keeps to itself (`View`, `Handle`, `HandleK`,
 * `HandleState`, `Perform`): a public `Cont.view(rm, p)` and handler constructors in okay-core would take it out.
 */
private[simple] object OnCore extends Simple {
  type AnyOp[X] = Any
  type P[A] = Cont[Row, Nothing, Nothing, A]

  type ![+A, R] = Cont[Row, Nothing, Nothing, A]
  type Pure = Nothing
  type +[R, E] = Nothing
  type Split[E, R, Rest] = Removed[AnyOp, Row, Row]
  type In[E, R] = okay.one.row.In[AnyOp, Row]
  type Sub[R, S] = Vector[Int]

  private def place(rm: Removed[AnyOp, Row, Row]): okay.one.row.In[AnyOp, Row] = rm match {
    case _: Removed.Head[?, ?] => In.Here[AnyOp, Row]().asInstanceOf[okay.one.row.In[AnyOp, Row]]
    case n: Removed.Next[?, ?, ?, ?] => In.There[AnyOp, AnyOp, Row](place(n.rm.asInstanceOf[Removed[AnyOp, Row, Row]]))
      .asInstanceOf[okay.one.row.In[AnyOp, Row]]
    case other => throw new IllegalStateException(s"not a place: $other")
  }
  private def placeAt(i: Int): okay.one.row.In[AnyOp, Row] =
    if (i == 0) In.Here[AnyOp, Row]().asInstanceOf[okay.one.row.In[AnyOp, Row]]
    else In.There[AnyOp, AnyOp, Row](placeAt(i - 1)).asInstanceOf[okay.one.row.In[AnyOp, Row]]
  private def depth(in: okay.one.row.In[AnyOp, Row]): Int = in match {
    case _: In.Here[?, ?] => 0
    case t: In.There[?, ?, ?] => 1 + depth(t.in.asInstanceOf[okay.one.row.In[AnyOp, Row]])
    case other => throw new IllegalStateException(s"not a place: $other")
  }

  private val head: Removed[AnyOp, Row, Row] = Removed.Head[AnyOp, Row]().asInstanceOf[Removed[AnyOp, Row, Row]]
  implicit def splitOne[E]: Split[E, E, Pure] = head
  implicit def splitLast[R, E]: Split[E, R + E, R] = head
  implicit def splitInit[E, R, G, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Removed.Next[AnyOp, AnyOp, Row, Row](s).asInstanceOf[Removed[AnyOp, Row, Row]]
  implicit def in[E, R, Rest](implicit s: Split[E, R, Rest]): In[E, R] = place(s)
  implicit def subPure[S]: Sub[Pure, S] = Vector()
  implicit def subOne[E, S](implicit e: In[E, S]): Sub[E, S] = Vector(depth(e))
  implicit def subMore[R, E, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = depth(e) +: r

  def pure[A, R](a: A): P[A] = Cont.Return(a)
  def flatMap[A, B, R](p: P[A])(f: A => P[B]): P[B] = p.flatMap(f)
  def delay[A, R](p: => P[A]): P[A] = Cont.Defer(() => p)
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: In[E, R]): P[X] = in match {
    case _: In.Here[?, ?] => Cont.PerformHere[AnyOp, Row, Nothing, X](op).asInstanceOf[P[X]]
    case _ => Cont.Perform[Row, AnyOp, Nothing, X](op, in)
  }

  def split[E, R, Rest, A](p: P[A])(implicit s: Split[E, R, Rest]): P[A] = s match {
    case _: Removed.Head[?, ?] => p
    case _ => Cont.View[AnyOp, Row, Row, A, Nothing, A, Nothing, Nothing](s, Machine.Frame.End(), p).asInstanceOf[P[A]]
  }

  /** each effect of `R`, from the head, translated to its place in `S`: the handlers further out take one off it */
  def embed[R, S, A](p: P[A])(implicit s: Sub[R, S]): P[A] = {
    val n = s.length
    s.indices.foldLeft(p) { (q, i) =>
      val to = placeAt(n - 1 - i + s(i))
      Cont.Handle[AnyOp, Row, A, Nothing, A, Nothing, Nothing, A](new Translate[AnyOp, Row, A] {
        def apply[X](op: Any): X ! Row = Cont.Perform[Row, AnyOp, Nothing, X](op, to)
      }, Machine.Frame.End(), q.asInstanceOf[Cont[AnyOp +: Row, Nothing, Nothing, A]]).asInstanceOf[P[A]]
    }
  }

  def handle[E, F[_], R, A, B](p: P[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): P[B] =
    Cont.HandleK[AnyOp, Row, A, Nothing, A, B](new Deep[AnyOp, Row, A, B] {
      def ret(a: A): B ! Row = h.ret(a)
      def apply[X](op: Any, k: X => B ! Row): B ! Row = h.op[X](op.asInstanceOf[F[X]], k)
    }, Machine.Frame.End(), p.asInstanceOf[Cont[AnyOp +: Row, Nothing, Nothing, A]])

  override def handleTail[E, F[_], R, A, B](p: P[A])(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): P[B] =
    Cont.Handle[AnyOp, Row, A, Nothing, A, Nothing, Nothing, B](new Answering[AnyOp, Row, A, B] {
      def ret(a: A): B ! Row = h.ret(a)
      def value[X](op: Any): X = h.answer[X](op.asInstanceOf[F[X]])
    }, Machine.Frame.End(), p.asInstanceOf[Cont[AnyOp +: Row, Nothing, Nothing, A]]).asInstanceOf[P[B]]

  /** okay-core's register: `value` then `next`, of one operation — the cell's answer, then the state it left */
  override def handleStateful[E, F[_], S, R, A, B](p: P[A])(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): P[B] =
    Cont.HandleState[AnyOp, Row, S, A, Nothing, A, Nothing, Nothing, B](new Stateful[AnyOp, Row, S, A, B] {
      private val cell = new Cell[S] { var s: S = s0; def get: S = s; def set(t: S): Unit = s = t }
      def initial: S = s0
      def ret(s: S, a: A): B ! Row = h.ret(s, a)
      def value[X](s: S, op: Any): X = { cell.set(s); h.answer[X](cell, op.asInstanceOf[F[X]]) }
      def next[X](s: S, op: Any): S = cell.get
    }, s0, Machine.Frame.End(), p.asInstanceOf[Cont[AnyOp +: Row, Nothing, Nothing, A]]).asInstanceOf[P[B]]

  def run[A](p: P[A]): A = Machine.run(p.asInstanceOf[Cont[okay.one.row.Pure, Nothing, Nothing, A]])
}

/** okay-simple's interface on okay-core's machine, seen through the interface only */
object OnCoreEffects {
  val effects: Simple = OnCore
}
