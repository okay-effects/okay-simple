package okay.simple.machine

import okay.simple.Effect
import scala.annotation.tailrec

/**
 * SCALA 3'S PLACES: a place is a typed path — the evidence `Split` itself — `LastPlace` the last effect of `R + E`,
 * `InitPlace` one further in, `OnePlace` a row of one effect. The walk to an operation's handler matches the path
 * against the levels, each indexed by its row: a handler's level of `E` at `T + E` met by a `LastPlace` at the same
 * row is, by the types, the operation's own handler — Scala 3 refines the types by the match. NO CAST.
 */
private[machine] trait Places extends MachineTypes {
  /** the place of `E` in `R`, and `R` without it `Rest` */
  sealed abstract class Place[E <: Effect, R, Rest] {
    /** the places of `R` renamed to `Rest + E`'s: `E` moved to the end */
    def toEnd: Renaming[R, Plus[Rest, E]]
    /** `R` is `Rest + E` already: no renaming */
    def same: (R =:= Plus[Rest, E]) | Null
  }
  final class OnePlace[E <: Effect] extends Place[E, E, PureRow] {
    def toEnd: Renaming[E, Plus[PureRow, E]] = new Renaming[E, Plus[PureRow, E]] {
      def apply[E2 <: Effect](at: Place[E2, E, ?]): Place[E2, Plus[PureRow, E], ?] = at match {
        case _: OnePlace[e] => new LastPlace[PureRow, E]
        case _ => throw new IllegalStateException(s"not a place in a row of one: $at")
      }
    }
    def same: Null = null
  }
  final class LastPlace[R, E <: Effect] extends Place[E, Plus[R, E], R] {
    def toEnd: Renaming[Plus[R, E], Plus[R, E]] = new Renaming[Plus[R, E], Plus[R, E]] {
      def apply[E2 <: Effect](at: Place[E2, Plus[R, E], ?]): Place[E2, Plus[R, E], ?] = at
    }
    def same: Plus[R, E] =:= Plus[R, E] = summon[Plus[R, E] =:= Plus[R, E]]
  }
  final class InitPlace[E <: Effect, R, G, Rest](val s: Place[E, R, Rest]) extends Place[E, Plus[R, G], Plus[Rest, G]] {
    def toEnd: Renaming[Plus[R, G], Plus[Plus[Rest, G], E]] = new Renaming[Plus[R, G], Plus[Plus[Rest, G], E]] {
      def apply[E2 <: Effect](at: Place[E2, Plus[R, G], ?]): Place[E2, Plus[Plus[Rest, G], E], ?] = at match {
        // `G`, the last: one in now, `E` after it
        case _: LastPlace[r, g] => new InitPlace[G, Plus[Rest, G], E, Rest](new LastPlace[Rest, G])
        case ip: InitPlace[e2, r, g, rest] => s.toEnd(ip.s) match {
          case _: LastPlace[r3, e3] => new LastPlace[Plus[Rest, G], E]
          case ip2: InitPlace[e4, r4, g4, rest4] => new InitPlace(new InitPlace[E2, Rest, G, rest4](ip2.s))
          case other => throw new IllegalStateException(s"not a place: $other")
        }
        case other => throw new IllegalStateException(s"not a place: $other")
      }
    }
    def same: Null = null
  }

  /** places renamed from `R1` to `R2`, typed */
  trait Renaming[R1, R2] { def apply[E <: Effect](at: Place[E, R1, ?]): Place[E, R2, ?] }

  type At[E <: Effect, R] = Place[E, R, ?]
  type Rename[R1, R2] = Renaming[R1, R2]
  type Split[E <: Effect, R, Rest] = Place[E, R, Rest]
  type In[E <: Effect, R] = Place[E, R, ?]
  type Sub[R, S] = Renaming[R, S]

  implicit def splitOne[E <: Effect]: Split[E, E, Pure] = new OnePlace[E]
  implicit def splitLast[R, E <: Effect]: Split[E, R + E, R] = new LastPlace[R, E]
  implicit def splitInit[E <: Effect, R, G, Rest](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    new InitPlace[E, R, G, Rest](s)
  implicit def in[E <: Effect, R, Rest](implicit s: Split[E, R, Rest]): In[E, R] = s
  implicit def subPure[S]: Sub[Pure, S] = new Renaming[PureRow, S] {
    def apply[E <: Effect](at: Place[E, PureRow, ?]): Place[E, S, ?] = throw new IllegalStateException("no place in Pure")
  }
  implicit def subOne[E <: Effect, S](implicit e: In[E, S]): Sub[E, S] = new Renaming[E, S] {
    def apply[E2 <: Effect](at: Place[E2, E, ?]): Place[E2, S, ?] = at match {
      case _: OnePlace[e] => e
      case other => throw new IllegalStateException(s"not a place in a row of one: $other")
    }
  }
  implicit def subMore[R, E <: Effect, S](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = new Renaming[Plus[R, E], S] {
    def apply[E2 <: Effect](at: Place[E2, Plus[R, E], ?]): Place[E2, S, ?] = at match {
      case _: LastPlace[r1, e1] => e
      case ip: InitPlace[e2, r2, g, rest] => r(ip.s)
      case other => throw new IllegalStateException(s"not a place: $other")
    }
  }

  def placeOf[E <: Effect, R](in: In[E, R]): At[E, R] = in
  def opAs[E <: Effect, X](op: Any): Op[E, X] = throw new IllegalStateException("Scala 3's places are typed")

  def split[E <: Effect, R, Rest, A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Plus[Rest, E], A] = s.same match {
    case ev: (R =:= Plus[Rest, E]) => ev.substituteCo[[r] =>> Prog[r, A]](p)
    case null => Remap(p, s.toEnd)
  }
  def embed[R, S, A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Remap(p, s)

  @tailrec final def levelOf[E <: Effect, R, V, Z](at: At[E, R], m: Meta[R, V, Z]): Handling[E] = m match {
    case l: HandlerLevel[e, t, a, b, w, z] => at match {
      case _: LastPlace[r, e2] => l
      case ip: InitPlace[e2, r, g, rest] => levelOf[E, t, w, Z](ip.s, l.m)
      case other => throw new IllegalStateException(s"not a place at a handler: $other")
    }
    case r: RemapLevel[r1, r2, a, w, z] => levelOf[E, r2, w, Z](r.rename(at), r.m)
    case _ => throw new IllegalStateException(s"an operation no handler took, at $at")
  }

  def capture[E <: Effect, R, X, V, Z](at: At[E, R], op: Op[E, X], fs: Frames[R, X, V], m: Meta[R, V, Z]): Next[Z] =
    walk[E, R, X, V, R, V, Z](at, op, fs, none[R, V], m)

  /** to the operation's general handler, the levels crossed taken onto `pre` */
  @tailrec private def walk[E <: Effect, R0, X, V0, R, V, Z](at: At[E, R], op: Op[E, X], inner: Frames[R0, X, V0],
      pre: Prefix[R0, V0, R, V], m: Meta[R, V, Z]): Next[Z] = m match {
    case l: HandlerLevel[e, t, a, b, w, z] => at match {
      case _: LastPlace[r, e2] => l.found[R0, X, V0](op, inner, pre)
      case ip: InitPlace[e2, r, g, rest] => walk[E, R0, X, V0, t, w, Z](ip.s, op, inner, l.crossed(pre), l.m)
      case other => throw new IllegalStateException(s"not a place at a handler: $other")
    }
    case r: RemapLevel[r1, r2, a, w, z] => walk[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, r.crossed(pre), r.m)
    case _ => throw new IllegalStateException(s"an operation no handler took, at $at")
  }
}
