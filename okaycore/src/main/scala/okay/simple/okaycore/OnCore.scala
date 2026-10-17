package okay.simple.okaycore

import okay.one.{Cont, !}
import okay.one.handler.{Answering, Deep, Stateful, Translate}
import okay.one.row.{+:, In, Plain, Pure, Removed, Row}
import okay.simple.{Ops, Simple}

/**
 * okay-simple's interface ON OKAY-CORE'S MACHINE, by okay-core's public API: a program is okay-core's own `Cont`,
 * nothing wrapped; okay-simple's places are okay-core's paths — the last effect of an okay-simple row is the head of
 * okay-core's — and `Split` is okay-core's `Removed`, made of the place: the head, or one further in. A handler is
 * one of okay-core's forms: a general one a `Deep`, one answering in place an `Answering`, a stateful one a
 * `Stateful` (the machine's register), installed by okay-core's `handle` at its `Removed` — the effect taken from
 * its place at once. A `split` on its own, or an `embed`, is a handler translating each operation to its new place.
 */
private[okaycore] object OnCore extends Simple {
  type AnyOp[X] = Any
  type P[A] = Cont[Row, Nothing, Nothing, A]
  type Place = okay.one.row.In[AnyOp, Row]
  type Taken = Removed[AnyOp, Row, Row]

  type ![+A, R] = Cont[Row, Nothing, Nothing, A]
  type Pure = Nothing
  type +[R, E] = Nothing
  type Split[E, R, Rest] = Slot
  type In[E, R] = Slot
  type Sub[R, S] = Vector[Int]

  private val plain: Plain[AnyOp] = summon[Plain[AnyOp]]
  /** THE TRUSTED STEPS, the only casts of the implementation: okay-simple's rows are phantom, so okay-core's are all
   * one, `Row` — a path, a `Removed`, a program seen at it (`atRow`); a program with nothing left, at `Pure` (`run`);
   * an operation found by its place, of its handler's operations (`opOf`) */
  private def atRow(in: okay.one.row.In[AnyOp, ? <: Row]): Place = in.asInstanceOf[Place]
  private def atRow(rm: Removed[AnyOp, ? <: Row, ? <: Row]): Taken = rm.asInstanceOf[Taken]
  private def atRow[A](p: Cont[? <: Row, Nothing, Nothing, A]): P[A] = p.asInstanceOf[P[A]]
  private def opOf[F[_], X](op: Any): F[X] = op.asInstanceOf[F[X]]

  private val head: Taken = atRow(Removed.Head[AnyOp, Row]())
  private val here: Place = atRow(okay.one.row.In.Here[AnyOp, Row]())

  /** a place, ONE PER DEPTH, shared: okay-core's path and its `Removed` made once — no allocation where an operation
   * is performed, and okay-core's look-up kept on the path (its cache) found again the next time */
  final class Slot(val depth: Int, val removed: Taken, val place: Place)
  @volatile private var slots: Array[Slot] = Array(new Slot(0, head, here))
  private def slot(i: Int): Slot = {
    val ss = slots
    if (i < ss.length) ss(i)
    else synchronized {
      var grown = slots
      while (grown.length <= i) {
        val last = grown(grown.length - 1)
        grown = grown :+ new Slot(last.depth + 1, atRow(Removed.Next[AnyOp, AnyOp, Row, Row](last.removed)),
          atRow(okay.one.row.In.There[AnyOp, AnyOp, Row](last.place)))
      }
      slots = grown
      grown(i)
    }
  }
  private def placeAt(i: Int): Place = slot(i).place
  implicit def splitOne[E]: Split[E, E, Pure] = slots(0)
  implicit def splitLast[R, E]: Split[E, R + E, R] = slots(0)
  implicit def splitInit[E, R, G, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] = slot(s.depth + 1)
  implicit def in[E, R, Rest](implicit s: Split[E, R, Rest]): In[E, R] = s
  implicit def subPure[S]: Sub[Pure, S] = Vector()
  implicit def subOne[E, S](implicit e: In[E, S]): Sub[E, S] = Vector(e.depth)
  implicit def subMore[R, E, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = e.depth +: r

  def pure[A, R](a: A): P[A] = Cont.pure(a)
  def flatMap[A, B, R](p: P[A])(f: A => P[B]): P[B] = p.flatMap(f)
  def delay[A, R](p: => P[A]): P[A] = Cont.defer(p)
  private def performAt[X](op: Any, at: Place): P[X] = Cont.perform[AnyOp, Row, X](op)(using at)
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: In[E, R]): P[X] = performAt[X](op, in.place)

  private def deep[F[_], A, B](h: Handler[F, ?, A, B]): Deep[AnyOp, Row, A, B] = new Deep[AnyOp, Row, A, B] {
    def ret(a: A): B ! Row = h.ret(a)
    def apply[X](op: Any, k: X => B ! Row): B ! Row = h.op[X](opOf[F, X](op), k)
  }
  private def answering[F[_], A, B](h: TailHandler[F, ?, A, B]): Answering[AnyOp, Row, A, B] = new Answering[AnyOp, Row, A, B] {
    def ret(a: A): B ! Row = h.ret(a)
    def value[X](op: Any): X = h.answer[X](opOf[F, X](op))
  }
  /** okay-core's register: `value` then `next`, of one operation — the cell's answer, then the state it left */
  private def stateful[F[_], S, A, B](s0: S, h: StatefulHandler[F, S, ?, A, B]): Stateful[AnyOp, Row, S, A, B] =
    new Stateful[AnyOp, Row, S, A, B] {
      private val cell = new Cell[S] { var s: S = s0; def get: S = s; def set(t: S): Unit = s = t }
      def initial: S = s0
      def ret(s: S, a: A): B ! Row = h.ret(s, a)
      def value[X](s: S, op: Any): X = { cell.set(s); h.answer[X](cell, opOf[F, X](op)) }
      def next[X](s: S, op: Any): S = cell.get
    }

  private def taking[A, B](p: P[A], h: Deep[AnyOp, Row, A, B], at: Taken): P[B] = p.handle(h)(using at, plain)
  private def passing[A, B](p: P[A], h: Answering[AnyOp, Row, A, B], at: Taken): P[B] = p.handle(h)(using at, plain)
  private def translating[A](p: P[A], h: Translate[AnyOp, Row, A], at: Taken): P[A] = p.handle(h)(using at, plain)
  private def keeping[S, A, B](p: P[A], h: Stateful[AnyOp, Row, S, A, B], at: Taken): P[B] = p.handle(h)(using at, plain)

  /** each operation of the effect taken at `at` performed again at `to`, in the row outside */
  private def moving[A](p: P[A], at: Taken, to: Place): P[A] = translating(p, new Translate[AnyOp, Row, A] {
    def apply[X](op: Any): X ! Row = performAt[X](op, to)
  }, at)

  /** a fresh head before the row, then the effect moved from its place, one further in now, to it */
  def split[E, R, Rest, A](p: P[A])(implicit s: Split[E, R, Rest]): P[A] =
    if (s.depth == 0) p else moving(atRow(Cont.widen[AnyOp, Row, A](p)), slot(s.depth + 1).removed, here)

  /** each effect of `R`, from the head, translated to its place in `S`: the handlers further out take one off it */
  def embed[R, S, A](p: P[A])(implicit s: Sub[R, S]): P[A] = {
    val n = s.length
    s.indices.foldLeft(p)((q, i) => moving(q, head, placeAt(n - 1 - i + s(i))))
  }

  def handleLast[E, F[_], R, A, B](p: P[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): P[B] = taking(p, deep(h), head)
  override def handleTailLast[E, F[_], R, A, B](p: P[A])(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): P[B] =
    passing(p, answering(h), head)
  override def handleStatefulLast[E, F[_], S, R, A, B](p: P[A])(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): P[B] = keeping(p, stateful(s0, h), head)

  override def handle[E, F[_], R, Rest, A, B](p: P[A])(h: Handler[F, Rest, A, B])(implicit s: Split[E, R, Rest],
      o: Ops.Aux[E, F]): P[B] = taking(p, deep(h), s.removed)
  override def handleTail[E, F[_], R, Rest, A, B](p: P[A])(h: TailHandler[F, Rest, A, B])(
      implicit s: Split[E, R, Rest], o: Ops.Aux[E, F]): P[B] = passing(p, answering(h), s.removed)
  override def handleStateful[E, F[_], S, R, Rest, A, B](p: P[A])(s0: S)(h: StatefulHandler[F, S, Rest, A, B])(
      implicit s: Split[E, R, Rest], o: Ops.Aux[E, F]): P[B] = keeping(p, stateful(s0, h), s.removed)

  def run[A](p: P[A]): A = p.asInstanceOf[Cont[okay.one.row.Pure, Nothing, Nothing, A]].run
}

/** okay-simple's interface on okay-core's machine, seen through the interface only */
object OnCoreEffects {
  val effects: Simple = OnCore
}
