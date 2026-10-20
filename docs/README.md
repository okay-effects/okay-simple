# okay-effects documentation

One interface of algebraic effects for Scala 3 and Scala 2.13 — programs whose effects are named in their type,
handlers that give them meaning, any number of implementations to run them on, on the JVM, Scala.js and Scala
Native.

Every code block here is compiled and its `assert`s run (`sbt docsCheck/test docsCheck2/test`), so what you read
runs.

## Start

- [Algebraic effects: what they are, and why](algebraic-effects.md) — an effect an interface, one program handled
  many ways, effects combined predictably, compared with monads; exceptions that can go back; how it works, by
  delimited continuations (`perform` a `shift`, `handle` a `reset`); a reading list.
- [Getting started](getting-started.md) — the library in your build, a first program, run on two implementations, in
  Scala 3 and in Scala 2.
- [A tour](tour.md) — eleven steps: rows, handling in any order, an effect of your own, a handler resuming twice,
  code generic in its row, choice, errors, generators, interception, delimited control, tagless.

## Examples and the family

- [Examples](examples.md) — the `examples/` module, shown and told: rows in any order, a handler resuming twice, a
  search, delimited control, code for both compilers, one application of both; and the best of the modules' pages.
- [The okay family](family.md) — okay-core, okay-kernel, okay (the platform), okay-java, okay-go, okay-rust,
  okay-python, okay-ts: what each is, an example of each, how each meets this interface.

## The guide

| Page | |
|---|---|
| [Imports and defaults](guide/imports.md) | `import okay.effects.*`, `.std.*`, `.delimited.*`, each module's package: the machine by default; another implementation |
| [Programs and rows](guide/programs-and-rows.md) | `A ! R`, effects as GADTs, rows `R + E`, the evidence `In`, `Split`, `Sub`; `Effects` and `Runner` |
| [Handlers](guide/handlers.md) | the five shapes — general, in place, aborting, stateful, stateful general — deep handlers, `k` |
| [The standard effects](guide/standard-effects.md) | `State`, `Reader`, `Writer`, `Error`, `Abort`, `Choose`, `Yield`, `Fresh`, `Random`, `Store`, `Lift` |
| [Interception](guide/interception.md) | `intercept`, `observe`, `around`: an effect's operations seen, changed, refused, the row kept |
| [Delimited control](guide/delimited-control.md) | `shift`/`reset` as an effect; `delimited`, the answer type changing |
| [Implementations](guide/implementations.md) | the machine, `Free`, tagless, okay-core's and okay-kernel's bridges; choosing; measured |
| [Scala 2 and Scala 3](guide/scala-2-and-3.md) | one interface, two ways of writing a row; code for both; one application of both |
| [Writing an implementation](guide/writing-an-implementation.md) | what to give, what to override, the interface's suite |
| [Platforms and the build](guide/platforms.md) | the JVM, Scala.js and Scala Native (when asked), the sbt commands, the layout |

## The modules

Each a module over the interface, for both compilers and both platforms, each checked on the machine, `Free` and
tagless. Its package is the module on the machine, ready: import it and write.

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.stream.*

type St = State % Int
assert(run(range[Pure](0, 5).map(_ * 2).toList) == List(0, 2, 4, 6, 8))
assert(run(state(0)(range[St](1, 4).evalMap(n => modify[Int, St](_ + n).andThen(get[Int, St])).toList)) == (6, List(1, 3, 6)))
```

On another implementation, a module is a class over the implementation in the implicit context —
`class Streams[F <: Effects](implicit implementation: F)` — made by `Streams()`, which keeps the implementation's
own type, so the programs it makes run on it: `given Runner = Free; val s = Streams(); import s.*, s.fx.*`.

| Module | |
|---|---|
| [stream](modules/stream.md) | lazy streams pulled by effects; transformations, `merge` in turn, to and from generators |
| [async](modules/async.md) | asynchrony with no runtime: callbacks driven to a `Future`, `sleep`, `both`, `race`, `timeout`; streams merged as they come |
| [concurrent](modules/concurrent.md) | cooperative threads in turns, a rendezvous channel |
| [resource](modules/resource.md) | scopes, `defer`, `bracket` |
| [io](modules/io.md) | the console, the clock, the environment as effects; the world's handlers and pure ones |
| [probability](modules/probability.md) | random choices: the exact distribution, or sampled |
| [parsing](modules/parsing.md) | parsers of an input, expectations and a choice: by the next character or backtracking, from a text or a stream, errors that say where |
| [json](modules/json.md) | `Json` the value; parsing an effect with two handlers, a recursive descent and the grammar on effects; values from a stream as they come |
| [algebra](modules/algebra.md) | the classes of computations and of combination: Functor to Monad, ParaMonad, Semigroup, Monoid, Foldable, the free applicative; programs as instances |
| [shape](modules/shape.md) | a typed schema: at once a form, a tool's input schema and the decoder; erased to a value; JSON Schema of it |
| [logic](modules/logic.md) | logic variables, unification, relations either way, a fair search |
| [transaction](modules/transaction.md) | transactions on a store, rolled back on an error |
| [resilience](modules/resilience.md) | retries and fallbacks |
| [interop](modules/interop.md) | cats (and cats-effect), ZIO and its streams, fs2, monix, Java's streams, tofu |
