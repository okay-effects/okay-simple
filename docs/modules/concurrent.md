# concurrent — cooperative threads

`okay-effects-concurrent`, `okay.effects.concurrent`. Threads — each a program — run in turns by a scheduler; a
thread runs until it passes the turn, sends or receives on a channel. No threads of the JVM, no runtime: a
scheduler is a handler.

| | |
|---|---|
| effect | `Coop[V, X]`, of one channel of values `V`: `Coop % V` in a row |
| operations | `pass` (the turn to the next), `send(v)` (waits for a receiver), `recv` (waits for a sender) — a rendezvous |
| scheduler | `scheduled[V](threads)`: each thread's value, `None` for one waiting forever |
| coroutine | `coroutine(p)`: a thread run to its next operation, a `Step` — `Done`, `Passed`, `Sending`, `Receiving` |

```scala
import okay.effects.machine.Machine
import okay.effects.concurrent.{Concurrent, Coop}

given okay.effects.Runner = Machine
val c = Concurrent()
import c.*, c.fx.*, c.fx.std.*

type T = Writer % String + Coop % Int

def worker(name: String, n: Int): Int ! T =
  if n == 0 then pure(0)
  else tell[String, T](s"$name$n").andThen(pass[Int, T]).andThen(worker(name, n - 1)).map(_ + 1)

val (log, results) = run(writer[String](scheduled[Int](List(worker("a", 2), worker("b", 3)))))
assert(log == List("a2", "b3", "a1", "b2", "b1"))
assert(results == List(Some(2), Some(3)))
```

A channel: a producer and a consumer meet at each value.

```scala
def producer(xs: List[Int]): Int ! T = xs.foldLeft(pure[Int, T](0))((p, x) => p.flatMap(n => send[Int, T](x).as(n + 1)))
def consumer(n: Int, sum: Int): Int ! T = if n == 0 then pure(sum) else recv[Int, T].flatMap(x => consumer(n - 1, sum + x))

assert(run(writer[String](scheduled[Int](List(producer(List(1, 2, 3)), consumer(3, 0)))))._2 == List(Some(3), Some(6)))
assert(run(writer[String](scheduled[Int](List(consumer(1, 0)))))._2 == List(None))      // no one sends: waits forever
```

## Semantics

- Round-robin from the first thread. A thread that sends is matched with the first receiver waiting, and both go on
  in turn; else it waits in the order it came.
- Structured: the threads are given together and `scheduled` ends when each has ended or waits forever. There is no
  `fork` from inside a thread: an effect carrying a program of its own row has a type that contains itself.
- Each thread's other effects are the row outside the scheduler: one `Writer` above serves them all.
- Constant stack: 50 threads of 200 turns each in the suite.
