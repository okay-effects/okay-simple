# async — asynchrony with no runtime

`okay-effects-async`, `okay.effects.async`. A program of `Async` waits on callbacks; `toFuture(p)` drives it to a
`Future`. No runtime of its own, no threads of its own: on the JVM and on Scala.js.

| | |
|---|---|
| effect | `Async`: `Suspend(register)`, waiting until `register`'s callback is called with a value or a failure |
| operations | `async(register)`, `fromFuture(f)`, `sleep(ms)` |
| combinators | `both(p, q)`, `parTraverse(xs)(f)`, `race(p, q)`, `timeout(ms)(p)` |
| run | `toFuture(p)`: a `Future` of the value, failed if a callback gave a failure or the program threw |
| streams | `AsyncStreams().mergeAsync(streams)` |

```scala
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import okay.effects.machine.Machine
import okay.effects.async.{Async, Asynchrony}

class Basics(using ExecutionContext):
  given okay.effects.Runner = Machine
  val as = Asynchrony()
  import as.*, as.fx.*, as.fx.std.*

  val p: Int ! Async = sleep[Async](10).andThen(fromFuture[Int, Async](Future(20))).map(_ + 22)
  def answer: Future[Int] = toFuture(p)

  /** another effect handled first, the row then `Async` alone — so `Async` the row's first, the state after it */
  type SA = Async + State % Int
  def tick(n: Int): Unit ! SA = if n == 0 then pure(()) else sleep[SA](1).andThen(modify[Int, SA](_ + 1)).andThen(tick(n - 1))
  def ticked: Future[(Int, Unit)] = toFuture(state(0)(tick(5)))

val basics = Basics(using ExecutionContext.global)
assert(Await.result(basics.answer, 5.seconds) == 42)
assert(Await.result(basics.ticked, 5.seconds) == (5, ()))
```

At once, and first:

```scala
class AtOnce(using ExecutionContext):
  given okay.effects.Runner = Machine
  val as = Asynchrony()
  import as.*, as.fx.*

  val slow = sleep[Async](300).as("slow")
  val fast = sleep[Async](5).as(7)
  def bothOf = toFuture(both(fast, sleep[Async](5).as("x")))
  def raced = toFuture(race(slow, fast))
  def timedOut = toFuture(timeout(50)(slow))
  def squares = toFuture(parTraverse((1 to 4).toList)(i => sleep[Async](5).as(i * i)))

val atOnce = AtOnce(using ExecutionContext.global)
assert(Await.result(atOnce.bothOf, 5.seconds) == (7, "x"))
assert(Await.result(atOnce.raced, 5.seconds) == Right(7))
assert(Await.result(atOnce.timedOut, 5.seconds) == None)
assert(Await.result(atOnce.squares, 5.seconds) == List(1, 4, 9, 16))
```

## How `toFuture` drives

A loop: the program runs to its next wait, and the callback is registered. A callback called while `register` runs
— a future already done — is taken there and the loop goes on, in constant stack; one called later resumes the rest
on the `ExecutionContext`. One `compareAndSet` decides which. `sleep` is the platform's timer: a scheduler thread on
the JVM, `setTimeout` on Scala.js.

## Streams merged as they come

`AsyncStreams().mergeAsync(streams)`: streams of `Async`, each one's next element pulled at once, all at the same
time; the merged stream gives whichever is ready first, then pulls the next of that stream. A stream that ends is
left out.

```scala
import okay.effects.async.AsyncStreams

class Merging(using ExecutionContext):
  given okay.effects.Runner = Machine
  val a = AsyncStreams()
  import a.*, a.fx.*, a.streams.*
  import a.asynchrony.{sleep, toFuture}

  def ticks(name: String, ms: Long, n: Int): Stream[String, Async] =
    range[Async](1, n + 1).evalMap(i => sleep[Async](ms).as(s"$name$i"))

  def merged: Future[List[String]] = toFuture(mergeAsync(List(ticks("a", 50, 3), ticks("b", 20, 3))).toList)

assert(Await.result(Merging(using ExecutionContext.global).merged, 5.seconds) == List("b1", "b2", "a1", "b3", "a2", "a3"))
```
