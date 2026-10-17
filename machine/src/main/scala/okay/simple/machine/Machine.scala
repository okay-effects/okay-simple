package okay.simple.machine

import okay.simple.{Ops, Simple}

/**
 * THE FAST IMPLEMENTATION: a machine of an explicit stack. The stack is the frames of the binds over the nearest
 * level, and the levels outside it, each a handler or a remapping of places (`split`, `embed`) with the frames that
 * were outside it. An operation carries its place in its row; the machine walks the levels outward, a remapping
 * renaming the place, a handler not its own taking one off it — nothing allocated, nothing performed again — to its
 * handler, place 0. There the rest up to the handler, the handler included, is `k`: the inner frames and the levels
 * crossed, immutable, so `k` is resumed any number of times; resumed, they are put back on the stack where the
 * resumption is, the handler's outside the frames there. Everything in a loop: constant stack.
 */
private[machine] object Machine extends Simple {
  sealed trait Prog[+A]
  final case class Done[A](a: A) extends Prog[A]
  final case class Perf[A](at: Int, op: Any) extends Prog[A]
  final case class Bind[X, A](p: Prog[X], f: X => Prog[A]) extends Prog[A]
  final case class Delay[A](p: () => Prog[A]) extends Prog[A]
  final case class Handle[A](p: Prog[Any], h: AnyHandler) extends Prog[A]
  final case class Remap[A](p: Prog[A], f: Int => Int) extends Prog[A]
  final case class Resume[A](k: K, x: Any) extends Prog[A]

  type AnyOp[X] = Any
  type AnyHandler = Handler[AnyOp, Any, Any, Any]
  type Frames = List[Any => Prog[Any]]
  /** a level: a handler (`h`) or a remapping (`f`), and the frames outside it */
  final case class Level(h: AnyHandler, f: Int => Int, outside: Frames)
  /** the rest up to a handler: the frames inside, the levels crossed (innermost first), the handler's level last */
  final case class K(inner: Frames, levels: List[Level])

  def eval[A](p: Prog[A]): A = {
    var current: Prog[Any] = p
    var frames: Frames = Nil
    var levels: List[Level] = Nil
    while (true) current match {
      case Bind(q, f) => frames = f.asInstanceOf[Any => Prog[Any]] :: frames; current = q
      case Done(a) =>
        if (frames.nonEmpty) { current = frames.head(a); frames = frames.tail }
        else if (levels.isEmpty) return a.asInstanceOf[A]
        else {
          val l = levels.head
          levels = levels.tail
          frames = l.outside
          current = if (l.h != null) l.h.ret(a) else Done(a)
        }
      case Delay(q) => current = q()
      case Handle(q, h) => levels = Level(h, null, frames) :: levels; frames = Nil; current = q
      case Remap(q, f) => levels = Level(null, f, frames) :: levels; frames = Nil; current = q
      case Perf(at0, op) =>
        var at = at0
        var crossed: List[Level] = Nil
        var ls = levels
        while (ls.nonEmpty && !(ls.head.h != null && at == 0)) {
          val l = ls.head
          if (l.h == null) at = l.f(at) else at -= 1
          crossed = l :: crossed
          ls = ls.tail
        }
        if (ls.isEmpty) throw new IllegalStateException(s"an operation no handler took: $op")
        val handler = ls.head
        val k = K(frames, (handler :: crossed).reverse)
        frames = handler.outside
        levels = ls.tail
        current = handler.h.op[Any](op, (x: Any) => Resume(k, x))
      case Resume(k, x) =>
        // the levels crossed put back as they were; the handler's, outside the frames of here
        var ls = levels
        var rev = k.levels.reverse
        ls = Level(rev.head.h, null, frames) :: ls
        rev = rev.tail
        while (rev.nonEmpty) { ls = rev.head :: ls; rev = rev.tail }
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
  type Split[E, R, Rest] = Place
  type In[E, R] = Place
  type Sub[R, S] = Vector[Int]

  implicit def splitOne[E]: Place = Place(0)
  implicit def splitLast[R, E]: Place = Place(0)
  implicit def splitInit[E, R, G, Rest](implicit s: Place): Place = Place(s.at + 1)
  implicit def in[E, R, Rest](implicit s: Place): Place = s
  implicit def subPure[S]: Vector[Int] = Vector()
  implicit def subOne[E, S](implicit e: Place): Vector[Int] = Vector(e.at)
  implicit def subMore[R, E, S](implicit r: Vector[Int], e: Place): Vector[Int] = e.at +: r

  def pure[A, R](a: A): Prog[A] = Done(a)
  def flatMap[A, B, R](p: Prog[A])(f: A => Prog[B]): Prog[B] = Bind(p, f)
  def delay[A, R](p: => Prog[A]): Prog[A] = Delay(() => p)
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: Place): Prog[X] = Perf(in.at, op)
  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = {
    val at = s.at
    if (at == 0) p else Remap(p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = Remap(p, s)
  def handle[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Handle(p, h.asInstanceOf[AnyHandler])
  def run[A](p: Prog[A]): A = eval(p)
}

/** the implementation, seen through the interface only */
object MachineEffects {
  val effects: Simple = Machine
}
