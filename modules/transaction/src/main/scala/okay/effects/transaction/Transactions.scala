package okay.effects.transaction

import okay.effects.{Effects, Std}

/** a transaction's operations on values `V` by key `K`: read, write. In a row `({ type L[x] = Tx[K, V, x] })#L` */
sealed trait Tx[K, V, X]
object Tx {
  final case class Read[K, V, X](k: K, answer: Option[V] => X) extends Tx[K, V, X]
  final case class Write[K, V, X](k: K, v: V, answer: Unit => X) extends Tx[K, V, X]
}

/**
 * TRANSACTIONS over any implementation `fx`, on the standard `Store` outside: `atomically(p)` keeps `p`'s writes in a
 * log of its own — a read sees the log first, then the store — and writes the log to the store when `p` ends. What
 * ends `p` before (an error raised and handled outside, an abort) drops the log: nothing written. `attempt[E]` outside
 * `atomically` is a transaction that rolls back on `E`.
 */
class Transactions[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation
   * to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  def read[K, V, R[_]](k: K)(implicit in: In[({ type L[x] = Tx[K, V, x] })#L, R]): Option[V] ! R =
    perform[({ type L[x] = Tx[K, V, x] })#L].apply[R, Option[V]](Tx.Read[K, V, Option[V]](k, v => v))
  def write[K, V, R[_]](k: K, v: V)(implicit in: In[({ type L[x] = Tx[K, V, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Tx[K, V, x] })#L].apply[R, Unit](Tx.Write[K, V, Unit](k, v, u => u))

  def atomically[K, V]: Atomically[K, V] = new Atomically[K, V]
  final class Atomically[K, V] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Tx[K, V, x] })#L, R, Rest],
      st: In[({ type L[x] = Std.Store[K, V, x] })#L, Rest]): A ! Rest =
      handle[({ type L[x] = Tx[K, V, x] })#L, Map[K, V], R, Rest, A, A](p, Map.empty[K, V],
        new StatefulHandler[({ type L[x] = Tx[K, V, x] })#L, Map[K, V], Rest, A, A] {
          def ret(log: Map[K, V], a: A) =
            log.foldLeft(pure[Unit, Rest](())) { case (q, (k, v)) => q.flatMap(_ => save[K, V, Rest](k, v)) }.as(a)
          def op[X](log: Map[K, V], op: Tx[K, V, X], k: (X, Map[K, V]) => A ! Rest) = op match {
            case Tx.Read(key, answer) => log.get(key) match {
                case Some(v) => k(answer(Some(v)), log)
                case None => load[K, V, Rest](key).flatMap(v => k(answer(v), log))
              }
            case Tx.Write(key, v, answer) => k(answer(()), log.updated(key, v))
          }
        })
  }
}

object Transactions {
  def apply()(implicit fx: Effects): Transactions[fx.type] = {
    implicit val exact: fx.type = fx
    new Transactions[fx.type]
  }
}
