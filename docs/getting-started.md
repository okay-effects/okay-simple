# Getting started

okay-effects is one interface of algebraic effects for Scala 3 and Scala 2.13. You write a program against the
interface — its effects named in its type, their handlers given later — and run it on any implementation: the
machine, the `Free` monad, tagless, okay-core's machine, okay-kernel's protocol. On the JVM, Scala.js and Scala
Native.

This page takes you from nothing to a program that runs. The [tour](tour.md) goes on from there.

## Get the library

Releases are a Maven repository on GitHub Pages: one resolver, and the modules you use.

```scala-sbt
resolvers += "okay-effects" at "https://okay-effects.github.io/okay-simple/maven"

libraryDependencies ++= Seq(
  "io.github.okay-effects" %% "okay-effects"         % "0.1.0",   // the interface and the standard effects
  "io.github.okay-effects" %% "okay-effects-machine" % "0.1.0"    // an implementation to run on
)
```

Any other implementation (`okay-effects-free`, `okay-effects-tagless`) and any module (`okay-effects-stream`,
`okay-effects-async`, ...) is one more line of the same form; `%%%` for Scala.js and Scala Native. The list of all
of them is the repository's index, [okay-effects.github.io/okay-simple](https://okay-effects.github.io/okay-simple/).

The latest code, unreleased: clone the repository and `sbt "+effects/publishLocal" "+machine/publishLocal"`, then the
version in `build.sbt` (a `-SNAPSHOT`).

## Scala versions

The libraries are built on Scala 3.3, the LTS, and on Scala 2.13: an application on any Scala 3 from 3.3 on, or on
Scala 2.13, can use them. The documentation's code is compiled on the latest Scala 3 (3.9), and writes two things
the LTS does not take:

| | Scala 3.3 LTS | newer Scala 3 |
|---|---|---|
| a row named by an alias | `type App[X] = (Reader % String + State % Int)[X]` | `type App = Reader % String + State % Int` (3.5 on) |
| several effects asked as context bounds | `R[_]: Has[Reader % String]: Has[State % Int]` | `R[_]: {Has[Reader % String], Has[State % Int]}` (3.6 on) |

Both forms of each mean the same, and the newer compilers take the LTS's forms too. kyo's interop
(`okay-effects-kyo`) is the one module built on the latest Scala 3, as kyo is.

## A first program

`import okay.effects.*` gives a program's types and words on the machine, the default implementation;
`import okay.effects.std.*` the standard effects. A program is a value of type `A ! R`: it gives a value `A`, and on
its way performs operations of the effects of the row `R`. Here a program reads a name from an environment (`Reader`) and counts the greetings in a state
(`State`):

```scala
import okay.effects.*
import okay.effects.std.*

/** greets, in any row that has a `Reader` of the name and a `State` of the count */
def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: String ! R =
  for
    name <- ask[String, R]
    n    <- get[Int, R]
    _    <- put[Int, R](n + 1)
  yield s"hello, $name (#${n + 1})"
```

`greet` is generic in its row `R`: it asks only that the row has the two effects (`Has[...]`, a context bound). Nothing
has happened yet — `greet` is a description. To run it, choose a row and give each effect its handler:

```scala
type App = Reader % String + State % Int

val handled: (Int, String) ! Pure = state(0)(reader("Ann")(greet[App]))
val result: (Int, String) = run(handled)

assert(result == (1, "hello, Ann (#1)"))
```

- `reader("Ann")` handles the `Reader`, answering every `ask` with `"Ann"`.
- `state(0)` handles the `State` from `0`, and gives the final state with the value: `(1, "hello, Ann (#1)")`.
- What is left is `Pure`, a program of no effect, and `run` runs it.

Each handler takes its effect from wherever it is in the row — the order you handle in is yours, not the row's.

Every module works so too: `import okay.effects.stream.*` gives the stream module on the machine,
`import okay.effects.async.*` asynchrony, `import okay.effects.delimited.*` delimited control — nothing else to set up.

## The same program on another implementation

Nothing in `greet` named the machine but its imports, the defaults. Write it over the interface, `Effects`, and choose the
implementation where you run:

```scala
import okay.effects.{Effects, Runner}

/** library code: it makes programs, over any implementation `fx` */
class Greeter[F <: Effects](val fx: F):
  import fx.*
  import fx.std.*
  def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: String ! R =
    for name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + 1) yield s"hello, $name"

/** the application's edge: it runs them, on the implementation it is given */
def greeted(fx: Runner): (Int, String) =
  val g = Greeter[fx.type](fx)
  import g.fx.*, g.fx.std.*
  run(state(0)(reader("Ann")(g.greet[Reader % String + State % Int])))

assert(greeted(okay.effects.machine.Machine) == (1, "hello, Ann"))
assert(greeted(okay.effects.free.Free) == (1, "hello, Ann"))
```

`Effects` makes programs; `Runner` (an `Effects` that runs) runs them. Every implementation is a `Runner`. Code that
makes programs takes an `Effects`; only the edge that runs them needs a `Runner`. (With `Greeter[fx.type]` the
programs `g` makes are of `fx`'s own types, so `fx` runs them.)

## The same program in Scala 2.13

Scala 2 has no type lambdas, so a row is written by names — `Reader % String` is a name, its constructor `#L`:

```scala2
import okay.effects._
import okay.effects.std._

object Greeting {
  def greet[R[_]](implicit r: In[(Reader % String)#L, R], s: In[(State % Int)#L, R]): String ! R =
    for { name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + 1) } yield s"hello, $name"

  type App[X] = ((Reader % String) + (State % Int))#L[X]
  val result: (Int, String) = run(state(0)(reader("Ann")(greet[App])))
  assert(result == ((1, "hello, Ann")))
}
assert(Greeting.result._1 == 1)
```

The interface is the same one; see [Scala 2 and Scala 3](guide/scala-2-and-3.md).

## Where next

- [Imports and defaults](guide/imports.md): what each import gives, and another implementation.
- [The tour](tour.md): effects of your own, handlers of each shape, resuming twice, generators, errors, interception.
- [The guide](README.md#the-guide): programs and rows, handlers, the standard effects, the implementations.
- [The modules](README.md#the-modules): streams, asynchrony, concurrency, resources, parsing, logic, and more.
