package okay.simple.machine


/**
 * SCALA 2'S PLACES: a place is a number, one object per depth, shared — no allocation where an operation is
 * performed. The walk to an operation's handler goes by it over the levels' fields. Scala 2 cannot refine types by
 * a match, so the handler found by a number is taken on trust: THREE TRUSTED STEPS, `handlerOf` (the handler found
 * is the operation's effect's), `opAs` (the operation is of its handler's operations), `sameRow` (a row whose last
 * effect is taken is that effect after the rest). The interface's evidence, found by the types of the row, makes
 * each so.
 */
private[machine] trait Places extends MachineTypes {
  final class Place(val n: Int)
  /** places renamed by number */
  final class Renaming(val f: Int => Int)

  type At[E[_], R[_]] = Place
  type Rename[R1[_], R2[_]] = Renaming
  type Split[E[_], R[_], Rest[_]] = Place
  type In[E[_], R[_]] = Place
  type Sub[R[_], S[_]] = Vector[Int]

  private val places: Array[Place] = Array.tabulate(64)(new Place(_))
  private def placeAt(i: Int): Place = if (i < places.length) places(i) else new Place(i)

  implicit def splitOne[E[_]]: Place = places(0)
  implicit def splitLast[R[_], E[_]]: Place = places(0)
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Place): Place = placeAt(s.n + 1)
  implicit def in[E[_], R[_], Rest[_]](implicit s: Place): Place = s
  implicit def subPure[S[_]]: Vector[Int] = Vector()
  implicit def subOne[E[_], S[_]](implicit e: Place): Vector[Int] = Vector(e.n)
  implicit def subMore[R[_], E[_], S[_]](implicit r: Vector[Int], e: Place): Vector[Int] = e.n +: r

  def placeOf[E[_], R[_]](in: Place): Place = in

  private def handlerOf[E[_]](l: Level0): Handling[E] = l.asInstanceOf[Handling[E]]
  def opAs[E[_], X](op: Any): Op[E, X] = op.asInstanceOf[Op[E, X]]
  private def sameRow[R[_], R2[_], A](p: Prog[R, A]): Prog[R2, A] = p.asInstanceOf[Prog[R2, A]]

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Place): Prog[Plus[Rest, E]#L, A] = {
    val at = s.n
    if (at == 0) sameRow[R, Plus[Rest, E]#L, A](p)
    else Remap[R, Plus[Rest, E]#L, A](p, new Renaming(i => if (i == at) 0 else if (i < at) i + 1 else i))
  }
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Vector[Int]): Prog[S, A] = Remap[R, S, A](p, new Renaming(s))

  /** the level answering place `at`, walked by the levels' fields */
  def levelOf[E[_], R[_], V, Z](at: Place, m: Meta[R, V, Z]): Handling[E] = {
    var n = at.n
    var l: Level0 = m
    while (n != 0 || (l.kind != GeneralKind && l.kind != TailKind && l.kind != StateKind)) {
      if (l.kind == RemapKind) l match {
        case r: RemapLevel[r1, r2, a, w, z] => n = r.rename.f(n)
        case _ => throw new IllegalStateException("not a remapping")
      }
      else if (l.kind == TopKind) throw new IllegalStateException(s"an operation no handler took, at ${at.n}")
      else n -= 1
      l = l.outer
    }
    handlerOf[E](l)
  }

  def capture[E[_], R[_], X, V, Z](at: Place, op: Op[E, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Next[Z] =
    walk[R, X, V, R, V, Z](at.n, op, fs, none[R, V], m)

  /** to the operation's general handler, the levels crossed taken onto `pre`; each step the level's own, typed */
  private def walk[R0[_], X, V0, R[_], V, Z](n: Int, op: Any, inner: Frames[R0, X, V0], pre: Prefix[R0, V0, R, V],
      m: Meta[R, V, Z]): Next[Z] =
    if (m.kind == RemapKind) {
      val n2 = m match {
        case r: RemapLevel[r1, r2, a, w, z] => r.rename.f(n)
        case _ => throw new IllegalStateException("not a remapping")
      }
      val o = m.stepOut(pre)
      walk[R0, X, V0, o.R, o.V, Z](n2, op, inner, o.pre, o.m)
    } else if (n == 0) m.foundAny[R0, X, V0](op, inner, pre)
    else {
      val o = m.stepOut(pre)
      walk[R0, X, V0, o.R, o.V, Z](n - 1, op, inner, o.pre, o.m)
    }
}
