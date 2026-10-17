package okay.effects.transaction

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class TransactionsChecks(val fx0: Runner) extends munit.FunSuite {
  val t = Transactions(fx0)
  import t._
  import t.fx._
  import t.fx.std._

  type Accounts[X] = Tx[String, Int, X]
  type Er[X] = Error[String, X]
  type Db[X] = Std.Store[String, Int, X]
  type T[X] = Sum[({ type L[x] = Sum[({ type M[y] = Sum[Void, Db, y] })#M, Er, x] })#L, Accounts, X]

  def balance(who: String): Int ! T = read[String, Int, T](who).map(_.getOrElse(0))
  def transfer(from: String, to: String, n: Int): Unit ! T =
    balance(from).flatMap(a => write[String, Int, T](from, a - n).flatMap(_ => balance(to)).flatMap(b => write[String, Int, T](to, b + n))
      .flatMap(_ => balance(from)).flatMap(left => if (left < 0) raise[String, T, Unit]("insufficient") else pure[Unit, T](())))

  val start = Map("ann" -> 10, "bob" -> 0)
  test("committed: the writes in the store at the end") {
    assertEquals(fx0.run(store[String, Int](start)(attempt[String](atomically[String, Int](transfer("ann", "bob", 7))))),
      (Map("ann" -> 3, "bob" -> 7), Right(())))
  }
  test("rolled back: an error raised inside, nothing written") {
    assertEquals(fx0.run(store[String, Int](start)(attempt[String](atomically[String, Int](transfer("ann", "bob", 20))))),
      (start, Left("insufficient")))
  }
}

class TransactionsOnMachine extends TransactionsChecks(Machine)
class TransactionsOnFree extends TransactionsChecks(Free)
