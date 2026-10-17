# interop — cats, ZIO, tofu

Programs of another monad inside a program are the standard `Lift[G, *]` (`lift(ga)`); a program of it given back as
a `G[A]` is a `Runner`'s `lower(p)`, given a `Lowering[G]`. The interop modules give the `Lowering`s, and more.

| Module | Platforms | What it gives |
|---|---|---|
| `okay-effects-cats` | JVM, Scala.js | `Cats().monad[R]`: cats' `Monad` of the programs, stack safe; `lowering[G]`: a `Lowering` of any cats `Monad` — cats-effect's `IO`, `Either`, `Option` |
| `okay-effects-zio` | JVM, Scala.js | `ZioEffects()`: `fromZIO(task)`, `toZIO(p)` — a task a `Lift[Task, *]`, a program a `Task` |
| `okay-effects-tofu` | JVM | `Tofu(Cats())`: tofu's `WithLocal` of a `Reader`, `Errors` of an `Error`, anywhere in a row |

## cats

```scala
import cats.Monad
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import okay.effects.machine.Machine
import okay.effects.cats.Cats

given okay.effects.Runner = Machine
val c = Cats()
import c.*, c.fx.*, c.fx.std.*

/** cats' Monad of the programs of a row: cats' code over any monad runs on the effects */
def twice[F[_]: Monad](fa: F[Int]): F[Int] = Monad[F].flatMap(fa)(a => Monad[F].map(fa)(_ + a))
type R = State % Int
val p: Int ! R = twice[c.Program[R]#L](modify[Int, R](_ + 1).andThen(get[Int, R]))
assert(run(state(0)(p)) == (2, 3))

/** cats-effect's IO lifted, lowered to an IO */
type LIO = [x] =>> Lift[IO, x]
val io = lift[IO, Int, LIO](IO(20)).flatMap(a => lift[IO, Int, LIO](IO(a + 22)))
assert(c.fx.lower[IO, Int](io).unsafeRunSync() == 42)
```

## ZIO

```scala
import zio.{Runtime, Unsafe, ZIO}
import okay.effects.zio.ZioEffects

object WithZio:
  given okay.effects.Runner = Machine
  val z = ZioEffects()
  import z.*, z.fx.*

  val p: Int ! Tasks = fromZIO[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIO[Int, Tasks](ZIO.attempt(a + 22)))
  val value: Int = Unsafe.unsafe { implicit u => Runtime.default.unsafe.run(toZIO(p)).getOrThrow() }

assert(WithZio.value == 42)
```

On Scala.js, ZIO's runtime needs `java.time`: add `"io.github.cquiroz" %%% "scala-java-time"`.

## tofu

`Tofu(Cats())` gives `withLocal[R, Rest, C]`, tofu's `WithLocal[Program[R]#L, C]` of a `Reader % C` in `R`, and
`errors[R, Rest, E]`, tofu's `Errors` (`raise`, `tryHandleWith`, `restore`) of an `Error % E` in `R` — so code
written to tofu's typeclasses runs on the effects. The JVM's only: tofu has no Scala.js build.
