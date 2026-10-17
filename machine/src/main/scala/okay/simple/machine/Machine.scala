package okay.simple.machine

import okay.simple.{Ops, Simple}

/**
 * THE FAST IMPLEMENTATION: a machine of an explicit stack. The stack is the frames of the binds over the nearest
 * level, and the levels outside it: a handler — general, answering in place, or answering in place with a state of
 * its own — or a remapping of places (`split`, `embed`), each with the frames that were outside it. An operation
 * carries its place in its row; the machine walks the levels outward, a remapping renaming the place, a handler not
 * its own taking one off — nothing allocated, nothing passed on — to its handler, place 0.
 *
 * A handler answering in place answers there and the run goes on: no `k`, the state, if any, written in its level.
 * A general handler's clause gets `k`: the frames inside and the levels crossed, a stateful level's state copied as
 * it is then — so `k`, resumed any number of times, resumes the state of its time; resumed, the levels are put back
 * on the stack where the resumption is (a stateful one fresh each time), the handler's outside the frames there.
 * Everything in one loop: constant stack.
 */
private[machine] object Machine extends Simple {
  sealed trait Prog[+A]
  final case class Done[A](a: A) extends Prog[A]
  final case class Perf[A](at: Int, op: Any) extends Prog[A]
  final case class Bind[X, A](p: Prog[X], f: X => Prog[A]) extends Prog[A]
  final case class Delay[A](p: () => Prog[A]) extends Prog[A]
  final case class Handle[A](p: Prog[Any], kind: Int, handler: AnyRef, s0: Any) extends Prog[A]
  final case class Resume[A](k: K, x: Any) extends Prog[A]

  type AnyOp[X] = Any
  type Frames = List[Any => Prog[Any]]

  final val General = 0
  final val Tail = 1
  final val Stateful = 2
  final val Remapping = 3

  /** a level: its kind, its handler (or its remapping), the frames outside it; a stateful one's state, its cell */
  final class Level(val kind: Int, val handler: AnyRef, val outside: Frames, var state: Any) extends Cell[Any] {
    def get: Any = state
    def set(s: Any): Unit = state = s
    /** as it is now, for a `k`: a stateful level's state copied */
    def taken: Level = if (kind == Stateful) new Level(kind, handler, outside, state) else this
  }
  /** the rest up to a handler: the frames inside, the levels crossed (innermost first), the handler */
  final class K(val inner: Frames, val crossed: Array[Level], val handler: AnyRef)

  def eval[A](p: Prog[A]): A = {
    var current: Prog[Any] = p
    var frames: Frames = Nil
    var levels: List[Level] = Nil
    while (true) current match {
      case Bind(q, f) => frames = f.asInstanceOf[Any => Prog[Any]] :: frames; current = q
      case Done(a) =>
        if (frames ne Nil) { current = frames.head(a); frames = frames.tail }
        else if (levels eq Nil) return a.asInstanceOf[A]
        else {
          val l = levels.head
          levels = levels.tail
          frames = l.outside
          current = (l.kind: @annotation.switch) match {
            case General => l.handler.asInstanceOf[Handler[AnyOp, Any, Any, Any]].ret(a)
            case Tail => l.handler.asInstanceOf[TailHandler[AnyOp, Any, Any, Any]].ret(a)
            case Stateful => l.handler.asInstanceOf[StatefulHandler[AnyOp, Any, Any, Any, Any]].ret(l.state, a)
            case _ => Done(a)
          }
        }
      case Perf(at0, op) =>
        var at = at0
        var crossed = 0
        var ls = levels
        var found: Level = null
        while (found == null) {
          if (ls eq Nil) throw new IllegalStateException(s"an operation no handler took: $op")
          val l = ls.head
          if (l.kind == Remapping) at = l.handler.asInstanceOf[Int => Int](at)
          else if (at == 0) found = l
          else at -= 1
          if (found == null) { crossed += 1; ls = ls.tail }
        }
        (found.kind: @annotation.switch) match {
          case Tail => current = Done(found.handler.asInstanceOf[TailHandler[AnyOp, Any, Any, Any]].answer[Any](op))
          case Stateful => current = Done(found.handler.asInstanceOf[StatefulHandler[AnyOp, Any, Any, Any, Any]].answer[Any](found, op))
          case _ =>
            val taken = new Array[Level](crossed)
            var i = 0
            var cs = levels
            while (i < crossed) { taken(i) = cs.head.taken; cs = cs.tail; i += 1 }
            val k = new K(frames, taken, found.handler)
            frames = found.outside
            levels = ls.tail
            current = found.handler.asInstanceOf[Handler[AnyOp, Any, Any, Any]].op[Any](op, (x: Any) => Resume(k, x))
        }
      case Delay(q) => current = q()
      case Handle(q, kind, h, s0) => levels = new Level(kind, h, frames, s0) :: levels; frames = Nil; current = q
      case Resume(k, x) =>
        var ls = new Level(General, k.handler, frames, null) :: levels
        var i = k.crossed.length - 1
        while (i >= 0) { ls = k.crossed(i).taken :: ls; i -= 1 }
        levels = ls
        frames = k.inner
        current = Done(x)
    }
    throw new IllegalStateException
  }

  type ![+A, R] = Prog[A]
  type Pure = Nothing
  type +[R, E] = Nothing
  final case class Place(at: Int)
  /** the places, one per depth, shared: no allocation where an operation is performed */
  private val places: Array[Place] = Array.tabulate(64)(Place(_))
  private def placeAt(i: Int): Place = if (i < places.length) places(i) else Place(i)
  type Split[E, R, Rest] = Place
  type In[E, R] = Place
  type Sub[R, S] = Vector[Int]

  implicit def splitOne[E]: Place = places(0)
  implicit def splitLast[R, E]: Place = places(0)
  implicit def splitInit[E, R, G, Rest](implicit s: Place): Place = placeAt(s.at + 1)
  implicit def in[E, R, Rest](implicit s: Place): Place = s
  implicit def subPure[S]: Vector[Int] = Vector()
  implicit def subOne[E, S](implicit e: Place): Vector[Int] = Vector(e.at)
  implicit def subMore[R, E, S](implicit r: Vector[Int], e: Place): Vector[Int] = e.at +: r

  def pure[A, R](a: A): Prog[A] = Done(a)
  def flatMap[A, B, R](p: Prog[A])(f: A => Prog[B]): Prog[B] = Bind(p, f)
  def delay[A, R](p: => Prog[A]): Prog[A] = Delay(() => p)
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: Place): Prog[X] = Perf(in.at, op)

  private def remap[A](p: Prog[A], f: Int => Int): Prog[A] = Handle(p, Remapping, f, null)
  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = {
    val at = s.at
    if (at == 0) p else remap(p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = remap(p, s)

  def handle[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Handle(p, General, h, null)
  override def handleTail[E, F[_], R, A, B](p: Prog[A])(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Handle(p, Tail, h, null)
  override def handleStateful[E, F[_], S, R, A, B](p: Prog[A])(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): Prog[B] = Handle(p, Stateful, h, s0)
  def run[A](p: Prog[A]): A = eval(p)
}

/** the implementation, seen through the interface only */
object MachineEffects {
  val effects: Simple = Machine
}
