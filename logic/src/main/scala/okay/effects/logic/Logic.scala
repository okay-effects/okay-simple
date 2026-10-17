package okay.effects.logic

import scala.collection.immutable.Queue
import okay.effects.{Effects, Std}

/** a term: a logic variable, an atom, a pair */
sealed trait Term
object Term {
  final case class Var(id: Long) extends Term
  final case class Atom(name: String) extends Term
  final case class Pair(head: Term, tail: Term) extends Term
  val empty: Term = Atom("nil")
  def list(ts: Term*): Term = ts.foldRight(empty)(Pair(_, _))
}

/** where a fair search stands: a value found, or the ways it can go on — each a program `M[...]` of the row outside */
sealed abstract class Search[A, M[_]]
object Search {
  final case class Found[A, M[_]](a: A) extends Search[A, M]
  final case class Ways[A, M[_]](ways: List[() => M[Search[A, M]]]) extends Search[A, M]
}

/**
 * LOGIC PROGRAMMING over any implementation `fx`, of standard effects: logic variables `fresh` ones (`Fresh`), the
 * bindings so far a state (`State[Subst]`), the alternatives `Choose` — `unify` binds, or fails. `solutions(p)` every
 * answer, the bindings inside the choice so each way has its own. And a FAIR search, `fair(n)(p)`: the ways tried
 * breadth-first, so a search with ways that never end still finds what the others give.
 */
class Logic[F <: Effects](val fx: F) {
  import fx._
  import fx.std._
  import Term._

  type Subst = Map[Long, Term]
  type Bindings[X] = State[Subst, X]

  def variable[R[_]](implicit f: In[Fresh, R]): Term ! R = fresh[R].map(Var(_))

  def walk(t: Term, s: Subst): Term = t match {
    case Var(id) => s.get(id) match { case Some(u) => walk(u, s); case None => t }
    case other => other
  }
  /** the term with every variable bound replaced, as far as the bindings go */
  def resolve(t: Term, s: Subst): Term = walk(t, s) match {
    case Pair(h, tl) => Pair(resolve(h, s), resolve(tl, s))
    case other => other
  }
  private def occurs(id: Long, t: Term, s: Subst): Boolean = walk(t, s) match {
    case Var(j) => j == id
    case Pair(h, tl) => occurs(id, h, s) || occurs(id, tl, s)
    case _ => false
  }
  def unifyIn(a: Term, b: Term, s: Subst): Option[Subst] = (walk(a, s), walk(b, s)) match {
    case (Var(i), Var(j)) if i == j => Some(s)
    case (Var(i), t) => if (occurs(i, t, s)) None else Some(s.updated(i, t))
    case (t, Var(j)) => if (occurs(j, t, s)) None else Some(s.updated(j, t))
    case (Atom(x), Atom(y)) => if (x == y) Some(s) else None
    case (Pair(h1, t1), Pair(h2, t2)) => unifyIn(h1, h2, s).flatMap(s1 => unifyIn(t1, t2, s1))
    case _ => None
  }
  /** `a` and `b` made the same, or this way fails */
  def unify[R[_]](a: Term, b: Term)(implicit st: In[Bindings, R], c: In[Choose, R]): Unit ! R =
    get[Subst, R].flatMap(s => unifyIn(a, b, s) match {
      case Some(s1) => put[Subst, R](s1)
      case None => fail[Unit, R]
    })
  /** one way or the other */
  def either[A, R[_]](p: => A ! R, q: => A ! R)(implicit c: In[Choose, R]): A ! R =
    among[Boolean, R](List(true, false)).flatMap(b => if (b) p else q)
  /** the term as bound now */
  def resolved[R[_]](t: Term)(implicit st: In[Bindings, R]): Term ! R = get[Subst, R].map(s => resolve(t, s))

  /** every answer of `p`, depth-first, fresh variables from 0 */
  def solutions[R[_], R1[_], R2[_], Rest[_], A](p: A ! R)(implicit b: Split[Bindings, R, R1], f: Split[Fresh, R1, R2],
      c: Split[Choose, R2, Rest]): List[A] ! Rest =
    all(freshFrom(0)(state[Subst](Map.empty)(p))).map(_.map(_._2))

  /** the first `n` values of `p`'s ways, tried breadth-first: a choice's ways each one step on in turn */
  def fair(n: Int): FairHandler = new FairHandler(n)
  final class FairHandler(n: Int) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest = {
      type M[x] = x ! Rest
      type S = Search[A, M]
      val tree: S ! Rest = handle[Choose, R, Rest, A, S](p, new Handler[Choose, Rest, A, S] {
        def ret(a: A) = pure(Search.Found[A, M](a))
        def op[X](op: Choose[X], k: X => S ! Rest) = op match {
          case Std.Choose.Among(xs) => pure(Search.Ways[A, M](xs.map(x => () => k(x))))
        }
      })
      def loop(queue: Queue[() => S ! Rest], found: List[A], left: Int): List[A] ! Rest =
        if (left == 0) pure(found.reverse)
        else queue.dequeueOption match {
          case None => pure(found.reverse)
          case Some((next, rest)) => next().flatMap {
            case Search.Found(a) => loop(rest, a :: found, left - 1)
            case Search.Ways(ways) => loop(rest ++ ways, found, left)
          }
        }
      loop(Queue(() => tree), Nil, n)
    }
  }
}

object Logic {
  def apply(fx: Effects): Logic[fx.type] = new Logic[fx.type](fx)
}
