package okay.simple.free

import okay.simple.{Ops, Simple}

/**
 * THE REFERENCE IMPLEMENTATION: a free monad. A row is an ordered list and an operation carries its place in it,
 * counted from the end: `split` moves a place to the end, `embed` maps places to a bigger row's, `handle` answers the
 * place 0 and moves the others one in. A program's next step is found by a loop over its binds, in constant stack;
 * a handler and a remapping are built when the run gets to them. `k` is a function to a program: resumed any number
 * of times. Simple first: every operation that crosses a handler is re-performed outside it.
 */
private[free] object Free extends Simple {
  sealed trait Prog[+A]
  final case class Done[A](a: A) extends Prog[A]
  final case class Perf[A](at: Int, op: Any) extends Prog[A]
  final case class Bind[X, A](p: Prog[X], f: X => Prog[A]) extends Prog[A]
  final case class Delay[A](p: () => Prog[A]) extends Prog[A]

  /** a program's next step: its value, or an operation at a place and the rest after it */
  sealed trait Step[+A]
  final case class Value[A](a: A) extends Step[A]
  final case class Operation[A](at: Int, op: Any, k: Any => Prog[A]) extends Step[A]

  def step[A](p: Prog[A]): Step[A] = {
    var current: Prog[Any] = p
    var rest: List[Any => Prog[Any]] = Nil
    while (true) current match {
      case Done(a) => rest match {
        case Nil => return Value(a.asInstanceOf[A])
        case f :: fs => current = f(a); rest = fs
      }
      case Bind(q, f) => rest = f.asInstanceOf[Any => Prog[Any]] :: rest; current = q
      case Delay(q) => current = q()
      case Perf(at, op) =>
        val fs = rest
        return Operation(at, op, (x: Any) => fs.foldLeft[Prog[Any]](Done(x))((q, f) => Bind(q, f)).asInstanceOf[Prog[A]])
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

  private def remap[A](p: Prog[A], f: Int => Int): Prog[A] = Delay(() => step(p) match {
    case Value(a) => Done(a)
    case Operation(at, op, k) => Bind(Perf[Any](f(at), op), (x: Any) => remap(k(x), f))
  })
  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] =
    remap(p, i => if (i == s.at) 0 else if (i < s.at) i + 1 else i)
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = remap(p, s)

  def handle[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    Delay(() => step(p) match {
      case Value(a) => h.ret(a)
      case Operation(0, op, k) => h.op(op.asInstanceOf[F[Any]], (x: Any) => handle[E, F, R, A, B](k(x))(h))
      case Operation(at, op, k) => Bind(Perf[Any](at - 1, op), (x: Any) => handle[E, F, R, A, B](k(x))(h))
    })

  def run[A](p: Prog[A]): A = step(p) match {
    case Value(a) => a
    case Operation(_, op, _) => throw new IllegalStateException(s"an operation no handler took: $op")
  }
}

/** the implementation, seen through the interface only: its types abstract to its users */
object FreeEffects {
  val effects: Simple = Free
}
