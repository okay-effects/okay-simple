package okay.simple.machine

import okay.simple.{Ops, Simple}
import scala.annotation.tailrec

/**
 * THE MACHINE, typed. The run is a typed loop over three things, each typed by what it carries:
 *
 *  - the program, `Prog[X]`, of the value `X`;
 *  - the FRAMES, `Frames[X, V]`: the binds from the value `X` to the value `V` of the nearest level's body —
 *    aligned, each bind's output the next one's input;
 *  - the LEVELS, `Meta[V, Z]`: from the value `V` of the nearest level's body to the run's value `Z` — a handler
 *    (general, answering in place, or with a state of its own) or a remapping of places (`split`, `embed`), each
 *    with the frames outside it.
 *
 * An operation carries its place in its row; it walks the levels outward, a remapping renaming its place, a handler
 * not its own taking one off — nothing allocated — to its handler, place 0. A handler answering in place answers
 * there, the run going on: no `k`, a state written in its level. A general handler's clause gets `k`: the frames
 * inside and the levels crossed, as a typed `Prefix` put back on the stack where `k` is resumed — a stateful level
 * with the state it had when `k` was taken, so `k`, resumed any number of times, resumes the state of its time.
 * Everything in one `@tailrec` loop: constant stack.
 *
 * ONE CAST, the trusted step (`opOf`): an operation found by its place is of its handler's operations. A place is a
 * number, not a type, so the compiler cannot see it; the interface's `Split` and `In`, found by the types of the
 * row, guarantee it.
 */
private[machine] object Machine extends Simple {
  sealed trait Prog[+A]
  final case class Done[A](a: A) extends Prog[A]
  final case class Bind[X, A](p: Prog[X], f: X => Prog[A]) extends Prog[A]
  final case class Delay[A](p: () => Prog[A]) extends Prog[A]
  final case class Perf[X](at: Int, op: Any) extends Prog[X]
  final case class HandleGeneral[F[_], R, A, B](p: Prog[A], h: Handler[F, R, A, B]) extends Prog[B]
  final case class HandleTail[F[_], R, A, B](p: Prog[A], h: TailHandler[F, R, A, B]) extends Prog[B]
  final case class HandleState[F[_], S, R, A, B](p: Prog[A], s0: S, h: StatefulHandler[F, S, R, A, B]) extends Prog[B]
  final case class Remap[A](p: Prog[A], f: Int => Int) extends Prog[A]
  final case class Resume[X, B](k: K[X, B], x: X) extends Prog[B]

  /** the binds from a value `X` to the value `V` of the nearest level's body */
  sealed abstract class Frames[-X, +V] {
    /** the value at the end of the frames: of `Last` only */
    def last(x: X): V
  }
  final class Last[V] extends Frames[V, V] { def last(x: V): V = x }
  final class Then[X, Y, V](val f: X => Prog[Y], val rest: Frames[Y, V]) extends Frames[X, V] {
    def last(x: X): V = throw new IllegalStateException("not the last frame")
  }

  /** the levels from the value `V` of the nearest one's body to the run's value `Z` */
  /** `kind`: the top, a handler's level or a remapping; `outer` the level further out, `null` at the top; `remap` a
   * remapping's renaming of places — fields, read by the walk to an operation's handler with no call */
  sealed abstract class Meta[-V, +Z](final val kind: Int, final val outer: Meta[Nothing, Any], final val remap: Int => Int) {
    /** the run's value: of `Top` only */
    def top(v: V): Z
  }
  final val TopKind = 0
  final val HandlerKind = 1
  final val RemapKind = 2
  final class Top[Z] extends Meta[Z, Z](TopKind, null, null) {
    def top(v: Z): Z = v
  }
  /** a level of the body's value `A`, the frames outside it from `B` to `W`, the levels outside them */
  sealed abstract class Level[A, B, W, Z](kind: Int, remap: Int => Int, final val outside: Frames[B, W],
      final val m: Meta[W, Z]) extends Meta[A, Z](kind, m, remap) {
    def top(v: A): Z = throw new IllegalStateException("not the top")
    /** `pre` with this level after it: a `k` taken past it puts it back as it is now */
    def crossed[V0](pre: Prefix[V0, A]): Prefix[V0, W]
  }
  final class GeneralLevel[F[_], R, A, B, W, Z](val h: Handler[F, R, A, B], outside: Frames[B, W], m: Meta[W, Z])
      extends Level[A, B, W, Z](HandlerKind, null, outside, m) {
    def crossed[V0](pre: Prefix[V0, A]): Prefix[V0, W] = new Prefix[V0, W] {
      def put[Z2](m2: Meta[W, Z2]): Meta[V0, Z2] = pre.put(new GeneralLevel(h, outside, m2))
    }
  }
  final class TailLevel[F[_], R, A, B, W, Z](val h: TailHandler[F, R, A, B], outside: Frames[B, W], m: Meta[W, Z])
      extends Level[A, B, W, Z](HandlerKind, null, outside, m) {
    def crossed[V0](pre: Prefix[V0, A]): Prefix[V0, W] = new Prefix[V0, W] {
      def put[Z2](m2: Meta[W, Z2]): Meta[V0, Z2] = pre.put(new TailLevel(h, outside, m2))
    }
  }
  /** a stateful handler's level: its state the cell its clauses write */
  final class StateLevel[F[_], S, R, A, B, W, Z](val h: StatefulHandler[F, S, R, A, B], var state: S,
      outside: Frames[B, W], m: Meta[W, Z]) extends Level[A, B, W, Z](HandlerKind, null, outside, m) with Cell[S] {
    def get: S = state
    def set(s: S): Unit = state = s
    /** put back with the state it has now, each time */
    def crossed[V0](pre: Prefix[V0, A]): Prefix[V0, W] = {
      val now = state
      new Prefix[V0, W] { def put[Z2](m2: Meta[W, Z2]): Meta[V0, Z2] = pre.put(new StateLevel(h, now, outside, m2)) }
    }
  }
  final class RemapLevel[A, W, Z](val f: Int => Int, outside: Frames[A, W], m: Meta[W, Z])
      extends Level[A, A, W, Z](RemapKind, f, outside, m) {
    def crossed[V0](pre: Prefix[V0, A]): Prefix[V0, W] = new Prefix[V0, W] {
      def put[Z2](m2: Meta[W, Z2]): Meta[V0, Z2] = pre.put(new RemapLevel(f, outside, m2))
    }
  }

  /** levels taken off the stack, from the value `V` of the innermost one's body up to `A`: put back on any levels */
  trait Prefix[V, +A] { def put[Z](m: Meta[A, Z]): Meta[V, Z] }
  private def none[V]: Prefix[V, V] = new Prefix[V, V] { def put[Z](m: Meta[V, Z]): Meta[V, Z] = m }

  /** `k`, resumed with an `X` where the frames from `B` and the levels are: the rest put back, the handler's level
   * outside it, those frames outside that */
  trait K[X, B] { def at[W, Z](x: X, fs: Frames[B, W], m: Meta[W, Z]): Next[Z] }
  /** where the run goes on: a program, its frames, the levels — their types its own */
  abstract class Next[+Z] {
    type X
    type V
    val c: Prog[X]
    val fs: Frames[X, V]
    val m: Meta[V, Z]
  }
  private def next[X0, V0, Z](c0: Prog[X0], fs0: Frames[X0, V0], m0: Meta[V0, Z]): Next[Z] = new Next[Z] {
    type X = X0
    type V = V0
    val c = c0
    val fs = fs0
    val m = m0
  }

  /** THE TRUSTED STEP: an operation found by its place, of its handler's operations */
  private def opOf[F[_], X](op: Any): F[X] = op.asInstanceOf[F[X]]

  /** an operation to its general handler: the levels it crosses taken off as `k`, the clause run outside them */
  @tailrec private def capture[X, V0, V, Z](op: Any, at: Int, inner: Frames[X, V0], pre: Prefix[V0, V],
      m: Meta[V, Z]): Next[Z] = m match {
    case r: RemapLevel[a, w, z] => capture[X, V0, w, Z](op, r.f(at), inner, r.crossed(pre), r.m)
    case g: GeneralLevel[f, r, a, b, w, z] if at == 0 =>
      val k = new K[X, b] {
        def at[W2, Z2](x: X, fs: Frames[b, W2], m2: Meta[W2, Z2]): Next[Z2] =
          next[X, V0, Z2](Done(x), inner, pre.put(new GeneralLevel(g.h, fs, m2)))
      }
      next[b, w, Z](g.h.op[X](opOf[f, X](op), (x: X) => Resume(k, x)), g.outside, g.m)
    case g: GeneralLevel[f, r, a, b, w, z] => capture[X, V0, w, Z](op, at - 1, inner, g.crossed(pre), g.m)
    case t: TailLevel[f, r, a, b, w, z] => capture[X, V0, w, Z](op, at - 1, inner, t.crossed(pre), t.m)
    case s: StateLevel[f, st, r, a, b, w, z] => capture[X, V0, w, Z](op, at - 1, inner, s.crossed(pre), s.m)
    case _ => throw new IllegalStateException(s"an operation no handler took: $op")
  }

  /** THE LOOP, its hot cases only — a bind, a value into a frame, an operation answered in place — small, so the JIT
   * compiles it whole; everything else a step of `cold`'s, the loop going on where it says */
  @tailrec private def loop[X, V, Z](c: Prog[X], fs: Frames[X, V], m: Meta[V, Z]): Z = c match {
    case d: Done[x] => fs match {
      case t: Then[a, y, v] => loop[y, V, Z](t.f(d.a), t.rest, m)
      case _ =>
        if (m.kind == TopKind) m.top(fs.last(d.a))
        else {
          val n = cold[X, V, Z](c, fs, m)
          loop[n.X, n.V, Z](n.c, n.fs, n.m)
        }
    }
    case b: Bind[y, x] => loop[y, V, Z](b.p, new Then[y, x, V](b.f, fs), m)
    case p: Perf[x] =>
      // the level answering the operation's place, by fields: no call
      var at = p.at
      var l: Meta[Nothing, Any] = m
      var k = l.kind
      while (at != 0 || k != HandlerKind) {
        if (k == HandlerKind) at -= 1
        else if (k == RemapKind) at = l.remap(at)
        else throw new IllegalStateException(s"an operation no handler took: ${p.op}")
        l = l.outer
        k = l.kind
      }
      l match {
        // answered in place, the answer straight into the next frame: no `Done` made
        case t: TailLevel[f, r, a, b, w, z] =>
          val x = t.h.answer[X](opOf[f, X](p.op))
          fs match {
            case th: Then[a2, y, v] => loop[y, V, Z](th.f(x), th.rest, m)
            case _ => loop[X, V, Z](Done(x), fs, m)
          }
        case s: StateLevel[f, st, r, a, b, w, z] =>
          val x = s.h.answer[X](s, opOf[f, X](p.op))
          fs match {
            case th: Then[a2, y, v] => loop[y, V, Z](th.f(x), th.rest, m)
            case _ => loop[X, V, Z](Done(x), fs, m)
          }
        case _ =>
          val n = cold[X, V, Z](c, fs, m)
          loop[n.X, n.V, Z](n.c, n.fs, n.m)
      }
    case d: Delay[x] => loop[X, V, Z](d.p(), fs, m)
    case _ =>
      val n = cold[X, V, Z](c, fs, m)
      loop[n.X, n.V, Z](n.c, n.fs, n.m)
  }

  /** the loop's cold steps: a level entered or left, an operation to a general handler, a `k` resumed */
  private def cold[X, V, Z](c: Prog[X], fs: Frames[X, V], m: Meta[V, Z]): Next[Z] = c match {
    case Done(x) =>
      val v = fs.last(x)
      m match {
        case l: GeneralLevel[f, r, a, b, w, z] => next[b, w, Z](l.h.ret(v), l.outside, l.m)
        case l: TailLevel[f, r, a, b, w, z] => next[b, w, Z](l.h.ret(v), l.outside, l.m)
        case l: StateLevel[f, s, r, a, b, w, z] => next[b, w, Z](l.h.ret(l.state, v), l.outside, l.m)
        case l: RemapLevel[a, w, z] => next[a, w, Z](Done(v), l.outside, l.m)
        case _ => throw new IllegalStateException("the top is the loop's")
      }
    case p: Perf[x] => capture[X, V, V, Z](p.op, p.at, fs, none[V], m)
    case h: HandleGeneral[f, r, a, b] => next[a, a, Z](h.p, new Last[a], new GeneralLevel(h.h, fs, m))
    case h: HandleTail[f, r, a, b] => next[a, a, Z](h.p, new Last[a], new TailLevel(h.h, fs, m))
    case h: HandleState[f, s, r, a, b] => next[a, a, Z](h.p, new Last[a], new StateLevel(h.h, h.s0, fs, m))
    case r: Remap[a] => next[a, a, Z](r.p, new Last[a], new RemapLevel(r.f, fs, m))
    case r: Resume[x, b] => r.k.at(r.x, fs, m)
    case _ => throw new IllegalStateException(s"not a cold step: $c")
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
  protected def performOp[E, F[_], R, X](op: F[X])(implicit in: Place): Prog[X] = Perf[X](in.at, op)

  def split[E, R, Rest, A](p: Prog[A])(implicit s: Place): Prog[A] = {
    val at = s.at
    if (at == 0) p else Remap(p, i => if (i == at) 0 else if (i < at) i + 1 else i)
  }
  def embed[R, S, A](p: Prog[A])(implicit s: Vector[Int]): Prog[A] = Remap(p, s)

  def handleLast[E, F[_], R, A, B](p: Prog[A])(h: Handler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    HandleGeneral(p, h)
  override def handleTailLast[E, F[_], R, A, B](p: Prog[A])(h: TailHandler[F, R, A, B])(implicit o: Ops.Aux[E, F]): Prog[B] =
    HandleTail(p, h)
  override def handleStatefulLast[E, F[_], S, R, A, B](p: Prog[A])(s0: S)(h: StatefulHandler[F, S, R, A, B])(
      implicit o: Ops.Aux[E, F]): Prog[B] = HandleState(p, s0, h)
  def run[A](p: Prog[A]): A = loop[A, A, A](p, new Last[A], new Top[A])
}

/** the implementation, seen through the interface only */
object MachineEffects {
  val effects: Simple = Machine
}
