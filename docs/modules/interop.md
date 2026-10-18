# interop — cats, ZIO, tofu

Programs of another monad inside a program are the standard `Lift[G, *]` (`lift(ga)`); a program of it given back as
a `G[A]` is a `Runner`'s `lower(p)`, given a `Lowering[G]`. The interop modules give the `Lowering`s, and more.

| Module | Platforms | What it gives |
|---|---|---|
| `okay-effects-cats` | JVM, Scala.js | `monad[R]` (`import okay.effects.catsinterop.*`): cats' `Monad` of the programs, stack safe; `lowering[G]`: a `Lowering` of any cats `Monad` — cats-effect's `IO`, `Either`, `Option` |
| `okay-effects-zio` | JVM, Scala.js | `fromZIO(task)`, `toZIO(p)` — a task a `Lift[Task, *]`, a program a `Task` |
| `okay-effects-java-stream` | JVM | `JavaStreams()`: Java's `Stream`s and `Iterator`s to and from the stream module's streams |
| `okay-effects-zio-stream` | JVM, Scala.js | `ZioStreams()`: a stream of ZIO's tasks a `ZStream`, and a `ZStream` a stream |
| `okay-effects-fs2` | JVM, Scala.js | `Fs2Streams()`: a stream of `F`'s programs (cats-effect's `IO`, any cats monad) an `fs2.Stream`, and back |
| `okay-effects-monix` | JVM, Scala.js | `Monix()`: monix's `Task` an effect; its `Observable` a stream, and back — monix 3 is of cats-effect 2 |
| `okay-effects-tofu` | JVM | `Tofu(Cats())`: tofu's `WithLocal` of a `Reader`, `Errors` of an `Error`, anywhere in a row |

## cats

```scala
import cats.Monad
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import okay.effects.*
import okay.effects.std.*
import okay.effects.catsinterop.*

/** cats' Monad of the programs of a row: cats' code over any monad runs on the effects */
def twice[F[_]: Monad](fa: F[Int]): F[Int] = Monad[F].flatMap(fa)(a => Monad[F].map(fa)(_ + a))
type R = State % Int
val p: Int ! R = twice[Program[R]#L](modify[Int, R](_ + 1).andThen(get[Int, R]))
assert(run(state(0)(p)) == (2, 3))

/** cats-effect's IO lifted, lowered to an IO: the `Lowering` of any cats monad found, `lowering` */
type LIO = [x] =>> Lift[IO, x]
val io = lift[IO, Int, LIO](IO(20)).flatMap(a => lift[IO, Int, LIO](IO(a + 22)))
assert(lower[IO, Int](io).unsafeRunSync() == 42)
```

## ZIO

```scala
import zio.{Runtime, Unsafe, ZIO}

object WithZio:
  import okay.effects.*
  import okay.effects.ziointerop.*

  val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIO[Int, Tasks](ZIO.attempt(a + 22)))
  val value: Int = Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(toZIO(p)).getOrThrow() }

assert(WithZio.value == 42)
```

On Scala.js, ZIO's runtime needs `java.time`: add `"io.github.cquiroz" %%% "scala-java-time"`.

## Java's streams

`fromJava(js)` makes a Java stream a stream of any row — made at the first pull, its elements pulled one at a time,
as far as asked. A Java stream is used once; this one is not: each step remembered, so pulled twice, the same
elements. `toJava(s)` and `toIterator(s)` go the other way, for a stream of no effect — lazily, each element pulled
when Java asks, so an endless stream is `limit`ed on Java's side.

```scala
import java.util.stream.{Collectors, Stream as JStream}

object WithJava:
  import okay.effects.*
  import okay.effects.std.*
  import okay.effects.stream.*
  import okay.effects.jstream.*

  val doubled: List[Int] = run(fromJava[Int, Pure](JStream.of(1, 2, 3, 4)).map(_ * 2).filter(_ > 2).toList)

  type St = State % Int
  val totals: (Int, List[Int]) = run(state(0)(fromJava[Int, St](JStream.of(1, 2, 3)).evalMap(n => modify[Int, St](_ + n).andThen(get[Int, St])).toList))

  val squares: String = toJava(iterate[Int, Pure](1)(_ + 1).map(n => n * n)).limit(4).collect(Collectors.toList[Int]()).toString

assert(WithJava.doubled == List(4, 6, 8))
assert(WithJava.totals == (6, List(1, 3, 6)))
assert(WithJava.squares == "[1, 4, 9, 16]")
```

## ZIO's streams

A stream of `Tasks` — its pulls performing ZIO's tasks — is a `ZStream`, and back. `toZStream(s)` makes each pull a
task; `toZStreamPure(s)` a stream of no effect; `fromZStream(zs)` a `ZStream` a stream of `Tasks`, pulled a chunk
at a time, as far as asked — its scope opened at the first pull and closed at its end or its failure, so a stream
not pulled to its end keeps it open: take what you need with `ZStream`'s own `take` when that matters.

```scala
import zio.stream.ZStream

object WithZioStreams:
  import okay.effects.*
  import okay.effects.stream.*
  import okay.effects.ziointerop.*
  import okay.effects.zstream.*

  private def unsafe[A](task: zio.Task[A]): A = Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(task).getOrThrow() }

  /** a stream of tasks, as a ZStream */
  val collected = unsafe(toZStream(range[Tasks](1, 4).evalMap(n => fromZIO[Int, Tasks](ZIO.succeed(n * 10)))).runCollect).toList
  /** a ZStream, transformed here */
  val evens = unsafe(toZIO(fromZStream(ZStream.range(0, 100).map(_ * 3)).filter(_ % 2 == 0).take(3).toList))

assert(WithZioStreams.collected == List(10, 20, 30))
assert(WithZioStreams.evens == List(0, 6, 12))
```

## fs2

A stream of `Lifted[F]` — its pulls performing `F`'s, cats-effect's `IO` or any `F` with a cats `Monad` — is an
`fs2.Stream[F, O]`, and back. `toFs2(s)` lowers each pull to an `F`; `toFs2Pure(s)` makes a stream of no effect an
fs2 stream of `Pure`; `fromFs2(fs)` makes an fs2 stream a stream here, a chunk at a time, as far as asked. Each step of
`fromFs2` is compiled on its own (`uncons`), so a resource the fs2 stream holds across its elements is released at
each step's end: compile such a stream on fs2's side, and lift what it gives.

```scala
object WithFs2:
  import okay.effects.*
  import okay.effects.std.*
  import okay.effects.stream.*
  import okay.effects.fs2interop.*

  type LIO = Lifted[IO]#L
  given okay.effects.Std.Lowering[IO] = catsInterop.lowering[IO]

  /** a stream of IO's, as an fs2 stream */
  val collected = toFs2(range[LIO](1, 4).evalMap(n => lift[IO, Int, LIO](IO(n * 10)))).compile.toList.unsafeRunSync()
  /** an fs2 stream, transformed here */
  val evens = lower[IO, List[Int]](fromFs2(_root_.fs2.Stream.range(0, 100).covary[IO].map(_ * 3)).filter(_ % 2 == 0).take(3).toList).unsafeRunSync()

assert(WithFs2.collected == List(10, 20, 30))
assert(WithFs2.evens == List(0, 6, 12))
```

## monix

`Monix()` makes monix's `Task` an effect (`fromTask`, `toTask`, the standard `Lift`) and its `Observable` a stream:
`toObservable(s)` each pull a task, `toObservablePure(s)` of no effect, `fromObservable(obs)` an `Observable` a stream
pulled as far as asked — through monix's `Iterant`, its back-pressure ours, subscribed at the first pull and released
at its end or its failure. Monix 3 is of cats-effect 2: it cannot be in one application with cats-effect 3 (fs2, the
cats module's checks).

```scala monix
import scala.concurrent.Await
import scala.concurrent.duration.*
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import okay.effects.*
import okay.effects.stream.*
import okay.effects.monixinterop.*

class WithMonix:
  given Scheduler = Scheduler.global

  /** a stream of tasks, as an Observable */
  def collected = toObservable(range[Tasks](1, 4).evalMap(n => fromTask[Int, Tasks](Task(n * 10)))).toListL.runToFuture
  /** an Observable, pulled as far as asked */
  def evens = toTask(fromObservable(Observable.range(0, 1000000).map(_.toInt * 3)).filter(_ % 2 == 0).take(3).toList).runToFuture

val withMonix = WithMonix()
assert(Await.result(withMonix.collected, 5.seconds) == List(10, 20, 30))
assert(Await.result(withMonix.evens, 5.seconds) == List(0, 6, 12))
```

## tofu

`Tofu(Cats())` gives `withLocal[R, Rest, C]`, tofu's `WithLocal[Program[R]#L, C]` of a `Reader % C` in `R`, and
`errors[R, Rest, E]`, tofu's `Errors` (`raise`, `tryHandleWith`, `restore`) of an `Error % E` in `R` — so code
written to tofu's typeclasses runs on the effects. The JVM's only: tofu has no Scala.js build.
