package okay.effects.concurrent

import scala.collection.immutable.Queue
import okay.effects.Effects

/** COOPERATIVE THREADS' OPERATIONS, of one channel of values `V`: `Pass` gives the turn to the next thread; `Send(v)`
 * waits for a receiver, `Recv` for a sender — a rendezvous. In a row `({ type L[x] = Coop[V, x] })#L`, Scala 3's
 * `Coop % V` */
sealed trait Coop[V, X]
object Coop {
  final case class Pass[V, X](answer: Unit => X) extends Coop[V, X]
  final case class Send[V, X](v: V, answer: Unit => X) extends Coop[V, X]
  final case class Recv[V, X](answer: V => X) extends Coop[V, X]
}

/** where a thread stopped: ended with its value, or waiting — its rest a program `M[...]` of the row outside */
sealed abstract class Step[V, A, M[_]]
object Step {
  final case class Done[V, A, M[_]](a: A) extends Step[V, A, M]
  final case class Passed[V, A, M[_]](k: Unit => M[Step[V, A, M]]) extends Step[V, A, M]
  final case class Sending[V, A, M[_]](v: V, k: Unit => M[Step[V, A, M]]) extends Step[V, A, M]
  final case class Receiving[V, A, M[_]](k: V => M[Step[V, A, M]]) extends Step[V, A, M]
}

/**
 * COOPERATIVE CONCURRENCY over any implementation `fx`: threads, each a program, run in turns by a scheduler — a
 * thread runs until it passes, sends or receives; then the next. Structured: the threads are given to `scheduled` together,
 * and `scheduled` ends when each has ended or waits forever (a deadlock: its result `None`). Each thread is a COROUTINE
 * (`coroutine`): its `Coop` handled to a `Step`, the rest of the thread its `k`, resumed once.
 *
 * No `fork` of a thread from inside one: an effect carrying a program of its own row is of a type that contains
 * itself.
 */
class Concurrent[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation to the code it is imported into */
  val fx: F = implementation
  import fx._

  def pass[V, R[_]](implicit in: In[({ type L[x] = Coop[V, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Coop[V, x] })#L].apply[R, Unit](Coop.Pass(u => u))
  def send[V, R[_]](v: V)(implicit in: In[({ type L[x] = Coop[V, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Coop[V, x] })#L].apply[R, Unit](Coop.Send(v, u => u))
  def recv[V, R[_]](implicit in: In[({ type L[x] = Coop[V, x] })#L, R]): V ! R =
    perform[({ type L[x] = Coop[V, x] })#L].apply[R, V](Coop.Recv(v => v))

  /** a program of the row `R` outside, of the value `X`: the rest of a thread */
  type Of[R[_]] = { type L[X] = X ! R }

  /** the thread `p` as a coroutine: run to its first `Coop` operation, or to its end */
  def coroutine[V, R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Coop[V, x] })#L, R, Rest]): Step[V, A, Of[Rest]#L] ! Rest =
    handle[({ type L[x] = Coop[V, x] })#L, R, Rest, A, Step[V, A, Of[Rest]#L]](p,
      new Handler[({ type L[x] = Coop[V, x] })#L, Rest, A, Step[V, A, Of[Rest]#L]] {
        def ret(a: A) = pure(Step.Done[V, A, Of[Rest]#L](a))
        def op[X](op: Coop[V, X], k: X => Step[V, A, Of[Rest]#L] ! Rest) = op match {
          case Coop.Pass(answer) => pure(Step.Passed[V, A, Of[Rest]#L](u => k(answer(u))))
          case Coop.Send(v, answer) => pure(Step.Sending[V, A, Of[Rest]#L](v, u => k(answer(u))))
          case Coop.Recv(answer) => pure(Step.Receiving[V, A, Of[Rest]#L](v => k(answer(v))))
        }
      })

  /** the threads run in turns, from the first: each one's value, or `None` if it waits forever */
  def scheduled[V]: Scheduler[V] = new Scheduler[V]
  final class Scheduler[V] {
    def apply[R[_], Rest[_], A](threads: List[A ! R])(implicit sp: Split[({ type L[x] = Coop[V, x] })#L, R, Rest]): List[Option[A]] ! Rest = {
      type S = Step[V, A, Of[Rest]#L]
      def loop(ready: Queue[(Int, S ! Rest)], senders: Queue[(Int, V, Unit => S ! Rest)], receivers: Queue[(Int, V => S ! Rest)],
          results: Map[Int, A]): Map[Int, A] ! Rest =
        ready.dequeueOption match {
          case None => pure(results)
          case Some(((i, now), rest)) => now.flatMap {
            case Step.Done(a) => loop(rest, senders, receivers, results.updated(i, a))
            case Step.Passed(k) => loop(rest.enqueue((i, k(()))), senders, receivers, results)
            case Step.Sending(v, k) => receivers.dequeueOption match {
              case Some(((j, rk), rs)) => loop(rest.enqueue((j, rk(v))).enqueue((i, k(()))), senders, rs, results)
              case None => loop(rest, senders.enqueue((i, v, k)), receivers, results)
            }
            case Step.Receiving(k) => senders.dequeueOption match {
              case Some(((j, v, sk), ss)) => loop(rest.enqueue((i, k(v))).enqueue((j, sk(()))), ss, receivers, results)
              case None => loop(rest, senders, receivers.enqueue((i, k)), results)
            }
          }
        }
      val started = Queue.empty[(Int, S ! Rest)] ++ threads.zipWithIndex.map { case (t, i) => (i, coroutine[V, R, Rest, A](t)) }
      loop(started, Queue.empty, Queue.empty, Map.empty).map(results => threads.indices.toList.map(results.get))
    }
  }
}

object Concurrent {
  def apply()(implicit fx: Effects): Concurrent[fx.type] = {
    implicit val exact: fx.type = fx
    new Concurrent[fx.type]
  }
}
