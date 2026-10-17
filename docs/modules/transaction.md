# transaction — transactions on a store

`okay-effects-transaction`, `okay.effects.transaction`. `atomically(p)` runs `p`'s reads and writes against a log of
its own over the standard `Store` outside — a read sees the log first, then the store — and writes the log to the
store when `p` ends. What ends `p` before drops the log: nothing written.

| | |
|---|---|
| effect | `Tx[K, V, X]`: in a row `[x] =>> Tx[K, V, x]` |
| operations | `read(k)`, `write(k, v)` |
| handler | `atomically[K, V](p)`, over a row with `Store[K, V, *]` |

```scala
import okay.effects.machine.Machine
import okay.effects.transaction.{Transactions, Tx}

given okay.effects.Runner = Machine
val t = Transactions()
import t.*, t.fx.*, t.fx.std.*

type Accounts = [x] =>> Tx[String, Int, x]
type Db = [x] =>> Store[String, Int, x]
type T = Db + Error % String + Accounts

def balance(who: String): Int ! T = read[String, Int, T](who).map(_.getOrElse(0))
def transfer(from: String, to: String, n: Int): Unit ! T =
  for
    a    <- balance(from)
    _    <- write[String, Int, T](from, a - n)
    b    <- balance(to)
    _    <- write[String, Int, T](to, b + n)
    left <- balance(from)
    _    <- if left < 0 then raise[String, T, Unit]("insufficient") else pure(())
  yield ()

val start = Map("ann" -> 10, "bob" -> 0)
assert(run(store[String, Int](start)(attempt[String](atomically[String, Int](transfer("ann", "bob", 7))))) ==
  (Map("ann" -> 3, "bob" -> 7), Right(())))
assert(run(store[String, Int](start)(attempt[String](atomically[String, Int](transfer("ann", "bob", 20))))) ==
  (start, Left("insufficient")))
```

`attempt` outside `atomically` is a transaction that rolls back on an error: the raise drops `atomically`'s rest,
its log with it, before anything is written.
