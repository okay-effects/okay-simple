package okay.effects.probability

import okay.effects.Effects

/** a random choice: `Weighted(xs)`, one of the values, each with its weight (positive, summing to anything) */
sealed trait Dist[X]
object Dist {
  final case class Weighted[X](xs: List[(X, Double)]) extends Dist[X]
}

/**
 * PROBABILITY over any implementation `fx`: a program of random choices (`flip`, `uniform`, `weighted`) — a model —
 * and its handlers: `exact`, every way it can go, each outcome with its probability, the same outcomes summed; and
 * `sampled`, one way, each choice made by the standard `Random` effect in the row outside.
 */
class Probability[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  def weighted[X, R[_]](xs: List[(X, Double)])(implicit in: In[Dist, R]): X ! R = perform[Dist].apply[R, X](Dist.Weighted(xs))
  def uniform[X, R[_]](xs: List[X])(implicit in: In[Dist, R]): X ! R = weighted[X, R](xs.map(x => (x, 1.0)))
  def flip[R[_]](p: Double)(implicit in: In[Dist, R]): Boolean ! R = weighted[Boolean, R](List((true, p), (false, 1 - p)))

  /** every outcome and its probability, the same outcomes summed, in the order first met */
  def exact[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Dist, R, Rest]): List[(A, Double)] ! Rest =
    handle[Dist, R, Rest, A, List[(A, Double)]](p, new Handler[Dist, Rest, A, List[(A, Double)]] {
      def ret(a: A) = pure(List((a, 1.0)))
      def op[X](op: Dist[X], k: X => List[(A, Double)] ! Rest) = op match {
        case Dist.Weighted(xs) =>
          val total = xs.map(_._2).sum
          xs.foldLeft(pure[List[(A, Double)], Rest](Nil)) { case (acc, (x, w)) =>
            acc.flatMap(sofar => k(x).map(outs => merge(sofar, outs.map { case (a, q) => (a, q * w / total) })))
          }
      }
    })
  private def merge[A](xs: List[(A, Double)], ys: List[(A, Double)]): List[(A, Double)] =
    ys.foldLeft(xs) { case (acc, (a, q)) =>
      if (acc.exists(_._1 == a)) acc.map { case (b, r) => if (b == a) (b, r + q) else (b, r) } else acc :+ ((a, q))
    }

  /** one way: each choice by `Random` outside, its weights its odds */
  def sampled[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Dist, R, Rest], rnd: In[Random, Rest]): A ! Rest =
    handle[Dist, R, Rest, A, A](p, new Handler[Dist, Rest, A, A] {
      def ret(a: A) = pure(a)
      def op[X](op: Dist[X], k: X => A ! Rest) = op match {
        case Dist.Weighted(xs) =>
          nextDouble[Rest].flatMap { u =>
            val target = u * xs.map(_._2).sum
            def pick(ys: List[(X, Double)], acc: Double): X = ys match {
              case (x, _) :: Nil => x
              case (x, w) :: t => if (acc + w > target) x else pick(t, acc + w)
              case Nil => sys.error("no values to choose from")
            }
            k(pick(xs, 0.0))
          }
      }
    })
}

object Probability {
  def apply()(implicit fx: Effects): Probability[fx.type] = {
    implicit val exact: fx.type = fx
    new Probability[fx.type]
  }
}
