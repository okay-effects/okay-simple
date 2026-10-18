# async — asynchrony with no runtime

`okay-effects-async`, `okay.effects.async` — `import okay.effects.async.*`, the module on the machine. A program of `Async` waits on callbacks; `toFuture(p)` drives it to a
`Future`. No runtime of its own, no threads of its own: on the JVM and on Scala.js.

| | |
|---|---|
| effect | `Async`: `Suspend(register)`, waiting until `register`'s callback is called with a value or a failure |
| operations | `async(register)`, `cancellable(register)`, `fromFuture(f)`, `sleep(ms)` |
| combinators | `both(p, q)`, `parTraverse(xs)(f)`, `race(p, q)`, `timeout(ms)(p)` |
| run | `toFuture(p)`: a `Future` of the value, failed if a callback gave a failure or the program threw; `start(p)`: a `Running`, its `future` and `cancel()` |
| streams | `mergeAsync(a, b, ...)` or `mergeAsync(list)`: streams of the default `okay.effects.stream` merged as they come |

```scala
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import okay.effects.*
import okay.effects.std.*
import okay.effects.async.*

class Basics:
  given ExecutionContext = ExecutionContext.global

  val p: Int ! Async = sleep[Async](10).andThen(fromFuture[Int, Async](Future(20))).map(_ + 22)
  def answer: Future[Int] = toFuture(p)

  /** another effect handled first, the row then `Async` alone — so `Async` the row's first, the state after it */
  type SA = Async + State % Int
  def tick(n: Int): Unit ! SA = if n == 0 then pure(()) else sleep[SA](1).andThen(modify[Int, SA](_ + 1)).andThen(tick(n - 1))
  def ticked: Future[(Int, Unit)] = toFuture(state(0)(tick(5)))

val basics = Basics()
assert(Await.result(basics.answer, 5.seconds) == 42)
assert(Await.result(basics.ticked, 5.seconds) == (5, ()))
```

At once, and first:

```scala
class AtOnce:

  val slow = sleep[Async](300).as("slow")
  val fast = sleep[Async](5).as(7)
  def bothOf = toFuture(both(fast, sleep[Async](5).as("x")))
  def raced = toFuture(race(slow, fast))
  def timedOut = toFuture(timeout(50)(slow))
  def squares = toFuture(parTraverse((1 to 4).toList)(i => sleep[Async](5).as(i * i)))

val atOnce = AtOnce()
assert(Await.result(atOnce.bothOf, 5.seconds) == (7, "x"))
assert(Await.result(atOnce.raced, 5.seconds) == Right(7))
assert(Await.result(atOnce.timedOut, 5.seconds) == None)
assert(Await.result(atOnce.squares, 5.seconds) == List(1, 4, 9, 16))
```

## Cancellation

`cancellable(register)` is a wait that can be stopped: `register` gives back how to stop it — a timer cleared, a
request aborted, a fiber interrupted. `start(p)` runs a program as `toFuture` does and gives a `Running`: its
`future`, and `cancel()` — the wait the program is in stopped, the rest never run, the future failed with
`Cancelled`. `sleep` is cancellable (its timer cleared); `race` and `timeout` cancel the loser, `both` and
`parTraverse` cancel the others when one fails.

```scala
class Cancelling:
  given ExecutionContext = ExecutionContext.global

  @volatile var stopped = false
  val never: Int ! Async = cancellable[Int, Async](_ => () => stopped = true)
  @volatile var after = false
  val running = start(never.map { n => after = true; n })
  running.cancel()

val cancelling = Cancelling()
assert(Await.ready(cancelling.running.future, 5.seconds).value.exists(_.failed.toOption.exists(_.isInstanceOf[Cancelled])))
assert(cancelling.stopped && !cancelling.after)
```

## How `toFuture` drives

A loop: the program runs to its next wait, and the callback is registered. A callback called while `register` runs
— a future already done — is taken there and the loop goes on, in constant stack; one called later resumes the rest
on the `ExecutionContext`. One `compareAndSet` decides which. `sleep` is the platform's timer: a scheduler thread on
the JVM, `setTimeout` on Scala.js.

## Streams merged as they come

`mergeAsync(streams)` (`AsyncStreams(...)` over streams of your own): streams of `Async`, each one's next element pulled at once, all at the same
time; the merged stream gives whichever is ready first, then pulls the next of that stream. A stream that ends is
left out.

```scala
import okay.effects.stream.*

class Merging:

  def ticks(name: String, ms: Long, n: Int): Stream[String, Async] =
    range[Async](1, n + 1).evalMap(i => sleep[Async](ms).as(s"$name$i"))

  def merged: Future[List[String]] = toFuture(mergeAsync(ticks("a", 200, 2), ticks("b", 20, 3)).toList)
  /** the same of a list */
  def mergedList: Future[List[String]] = toFuture(mergeAsync(List(ticks("a", 200, 2), ticks("b", 20, 3))).toList)

val merging = Merging()
assert(Await.result(merging.merged, 5.seconds) == List("b1", "b2", "b3", "a1", "a2"))
assert(Await.result(merging.mergedList, 5.seconds) == List("b1", "b2", "b3", "a1", "a2"))
```
