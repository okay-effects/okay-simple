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
  final case class Handle[A](p: Prog[Any], kind: Kind) extends Prog[A]
  final case class Resume[A](k: K, x: Any) extends Prog[A]

  /** the machine's view of a handler: its operations and values erased — `Any` */
  type AnyOp[X] = Any
  type AnyHandler = Handler[AnyOp, Any, Any, Any]
  type AnyTail = TailHandler[AnyOp, Any, Any, Any]
  type AnyStateful = StatefulHandler[AnyOp, Any, Any, Any, Any]
  type Frames = List[Any => Prog[Any]]

  /** what a `Handle` puts on the stack, each time the run enters it */
  sealed trait Kind
  final case class GeneralKind(h: AnyHandler) extends Kind
  final case class TailKind(h: AnyTail) extends Kind
  final case class StatefulKind(h: AnyStateful, s0: Any) extends Kind
  final case class RemapKind(f: Int => Int) extends Kind

  /** a level of the stack, with the frames outside it */
  sealed abstract class Level(val outside: Frames) {
    /** as it is now, for a `k`: a stateful level's state copied */
    def taken: Level = this
  }
  final class GeneralLevel(val h: AnyHandler, outside: Frames) extends Level(outside)
  final class TailLevel(val h: AnyTail, outside: Frames) extends Level(outside)
  final class RemapLevel(val f: Int => Int, outside: Frames) extends Level(outside)
  /** a stateful handler's level: its state the cell its clauses write */
  final class StateLevel(val h: AnyStateful, outside: Frames, var state: Any) extends Level(outside) with Cell[Any] {
    def get: Any = state
    def set(s: Any): Unit = state = s
    override def taken: Level = new StateLevel(h, outside, state)
  }
  private def level(kind: Kind, outside: Frames): Level = kind match {
    case GeneralKind(h) => new GeneralLevel(h, outside)
    case TailKind(h) => new TailLevel(h, outside)
    case StatefulKind(h, s0) => new StateLevel(h, outside, s0)
    case RemapKind(f) => new RemapLevel(f, outside)
  }

  /** the rest up to a handler: the frames inside, the levels crossed (innermost first), the handler */
  final class K(val inner: Frames, val crossed: Array[Level], val handler: AnyHandler)

  /**
   * THE MACHINE. Its trusted steps, the only casts of the implementation: a bind's function onto the frames, of
   * `Any` (`push`), the run's value, `A` (`result`), and a handler erased where it is installed (`erase`, three)
   */
  def eval[A](p: Prog[A]): A = {
    var current: Prog[Any] = p
    var frames: Frames = Nil
    var levels: List[Level] = Nil
    while (true) current match {
      case b: Bind[x, a] => frames = push(b.f) :: frames; current = b.p
      case Done(a) =>
        if (frames ne Nil) { current = frames.head(a); frames = frames.tail }
        else if (levels eq Nil) return result[A](a)
        else {
          val l = levels.head
          levels = levels.tail
          frames = l.outside
          current = l match {
            case g: GeneralLevel => g.h.ret(a)
            case t: TailLevel => t.h.ret(a)
            case st: StateLevel => st.h.ret(st.state, a)
            case _: RemapLevel => Done(a)
          }
        }
      case Perf(at0, op) =>
        var at = at0
        var crossed = 0
        var ls = levels
        var found: Level = null
        while (found == null) {
          if (ls eq Nil) throw new IllegalStateException(s"an operation no handler took: $op")
          ls.head match {
            case r: RemapLevel => at = r.f(at)
            case l => if (at == 0) found = l else at -= 1
          }
          if (found == null) { crossed += 1; ls = ls.tail }
        }
        found match {
          case t: TailLevel => current = Done(t.h.answer[Any](op))
          case st: StateLevel => current = Done(st.h.answer[Any](st, op))
          case g: GeneralLevel =>
            val taken = new Array[Level](crossed)
            var i = 0
            var cs = levels
            while (i < crossed) { taken(i) = cs.head.taken; cs = cs.tail; i += 1 }
            val k = new K(frames, taken, g.h)
            frames = g.outside
            levels = ls.tail
            current = g.h.op[Any](op, (x: Any) => Resume(k, x))
          case _: RemapLevel => throw new IllegalStateException
        }
      case Delay(q) => current = q()
      case Handle(q, kind) => levels = level(kind, frames) :: levels; frames = Nil; current = q
      case Resume(k, x) =>
        var ls: List[Level] = new GeneralLevel(k.handler, frames) :: levels
        var i = k.crossed.length - 1
        while (i >= 0) { ls = k.crossed(i).taken :: ls; i -= 1 }
        levels = ls
        frames = k.inner
        current = Done(x)
    }
    throw new IllegalStateException
  }

  private def push[X, B](f: X => Prog[B]): Any => Prog[Any] = f.asInstanceOf[Any => Prog[Any]]
  private def result[A](a: Any): A = a.asInstanceOf[A]
  private def erase[F[_], R, A, B](h: Handler[F, R, A, B]): AnyHandler = h.asInstanceOf[AnyHandler]
  private def erase[F[_], R, A, B](h: TailHandler[F, R, A, B]): AnyTail = h.asInstanceOf[AnyTail]
  private def erase[F[_], S, R, A, B](h: StatefulHandler[F, S, R, A, B]): AnyStateful = h.asInstanceOf[AnyStateful]

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

  private def remap[A](p: Prog[A], f: Int => Int): Prog[A] = Handle(p, RemapKind(f))
  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = {
    val at = s.at
    if (at == 0) p else remap(p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = remap(p, s)

  def handleLast[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Handle(p, GeneralKind(erase(h)))
  override def handleTailLast[E, F[_], R, A, B](p: Prog[A])(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Handle(p, TailKind(erase(h)))
  override def handleStatefulLast[E, F[_], S, R, A, B](p: Prog[A])(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): Prog[B] = Handle(p, StatefulKind(erase(h), s0))
  def run[A](p: Prog[A]): A = eval(p)
}

/** the implementation, seen through the interface only */
object MachineEffects {
  val effects: Simple = Machine
}
