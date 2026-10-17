# okay-simple

One simple interface for effect systems, for Scala 3 and Scala 2.13: the types `!`, `+`, `%`, the functions
`perform`, `handle`, `split`, `run`, and the standard effects written once on them. The interface is ONE for both
compilers, `Effectful`: a program, a row, the evidence that an effect is in a row are abstract types, and an
implementation makes them what it likes. Only how a row is written is each compiler's own `Effects`: in Scala 3 by
type lambdas, `Choose + State % Int`; in Scala 2 by names taken as the constructor they make,
`(*[Choose] + (State % Int))#L`.

Two implementations here — a machine, and the `Free` monad — and two bridges in other repositories, to okay-core's
machine and to okay-kernel's protocol, all checked by one suite.

## Modules and packages

| Module | Artifact | Package | What it is |
|---|---|---|---|
| `core/effects/` | `okay-effects` | `okay.effects` | the interface (`Effectful`, each compiler's `Effects`; `Runner`, an implementation that runs), the standard effects (`Std`), delimited control changing answer types (`Delimited`); typed places for Scala 3 implementations (`Places`) |
| `core/machine/` | `okay-effects-machine` | `okay.effects.machine` | THE MACHINE, one per compiler: `okay.effects.machine.Machine` |
| `core/free/` | `okay-effects-free` | `okay.effects.free` | THE `Free` MONAD, one per compiler: `okay.effects.free.Free` (through the interface), `OnFree` (as it is) |
| `tagless/` | `okay-effects-tagless` | `okay.effects.tagless` | TAGLESS: `Tagless`, an `Effects` of no implementation — a program a value, run on the `Runner` in the implicit context |
| `modules/`: `concurrent`, `resource`, `io`, `probability`, `parsing`, `logic`, `transaction`, `resilience`, `stream` | `okay-effects-concurrent`, ... | `okay.effects.concurrent`, ... | MORE EFFECTS, each over the interface: see below |
| `tck/` | `okay-effects-tck` | `okay.effects` | the interface's suite, a library: `InterfaceSuite(fx)` for any implementation |
| `interop/cats`, `interop/zio`, `interop/tofu` | `okay-effects-cats`, `-zio`, `-tofu` | `okay.effects.cats`, `.zio`, `.tofu` | interop with cats (and cats-effect 3), ZIO, tofu |
| `examples/` | — | `okay.effects.examples` | examples over `Effects` alone, run on the machine and on `Free` |
| `suite/`, `suite-free/`, `compat/`, `bench/` | — | — | the suite on each implementation, the compatibility checks, the benchmark |

In okay-core's repository, module `effects/` (`okay-effects-core`): the interface on okay-core's machine,
`okay.effects.core.Core`. In okay-kernel's repository, module `effects/` (`okay-effects-kernel`): the interface on
okay-kernel's protocol, `okay.effects.kernel.OnKernel.on(effects)`. Both need `okay-effects` and `okay-effects-tck`
published locally from here.

## A program

An implementation is a value of type `Effects`: its types and primitives one import, its standard effects
another, from its `std`.

Scala 3:

```scala
import okay.effects.machine.Machine.*
import okay.effects.machine.Machine.std.*

// in any row that has these three, at any place
def greet[R[_]: {Has[Reader % String], Has[Writer % String], Has[State % Int]}]: Int ! R =
  for
    name <- ask[String, R]
    _    <- tell[String, R](s"hello, $name")
    n    <- get[Int, R]
    _    <- put[Int, R](n + name.length)
  yield n

type Row = Reader % String + Writer % String + State % Int
run(reader("Ann")(state(10)(writer[String](greet[Row]))))             // (13, (List("hello, Ann"), 10))
```

Scala 2:

```scala
import okay.effects.machine.Machine._
import okay.effects.machine.Machine.std._

def greet[R[_]](implicit r: In[(Reader % String)#L, R], w: In[(Writer % String)#L, R], s: In[(State % Int)#L, R]): Int ! R =
  for {
    name <- ask[String, R]
    _    <- tell[String, R](s"hello, $name")
    n    <- get[Int, R]
    _    <- put[Int, R](n + name.length)
  } yield n

type Row[X] = ((Reader % String) + (Writer % String) + (State % Int))#L[X]    // names; each % in parentheses
run(reader("Ann")(state(10)(writer[String](greet[Row]))))
```

Code over any implementation takes it as a value, `class Greeting(implicit val fx: Effects) { import fx._, fx.std._
... }`, and runs on whichever it is given: `Machine`, `Free`, okay-core's `Core`, `OnKernel.on(...)`. `Effects` are
the words that MAKE programs; RUNNING one is a `Runner`'s (`Runner extends Effects`: `run`, and `lower`) — every
implementation is one; code that runs programs takes a `Runner`. So words of no implementation, `Tagless`, are an
`Effects` too.

## The types

| | Scala 3 | Scala 2 |
|---|---|---|
| a program of value `A` over the row `R[_]` | `A ! R` | the same |
| a row of no effect | `Pure` | the same; among names `*[Pure]` |
| a row of one effect, `E` | `E` | the same |
| the row `R` and `E` after it, left to right | `R + E` | `(R + E)#L`, of names |
| an effect of the operations `F[X]` | `F`: `Choose` | `F`; among names `*[F]` |
| an effect of the operations `F[S, X]`, with its parameter | `F % S`: `State % Int` | `(F % S)#L`; among names `F % S` |
| the operation of `E` answered by an `X` | `E[X]` | the same |
| `E` is in `R`; a context bound of it | `In[E, R]`; `Has[E]` | `In[E, R]`; `Has[E]#L` |
| `E` is in `R`, at any place, and `R` without it is `Rest` | `Split[E, R, Rest]` (an `In`) | the same |
| every effect of `R` is in `S` | `Sub[R, S]` | the same |

A row is a type constructor: `R + E` is the phantom `Sum[R, E, X]`, `Pure` the empty `Void` (of no value: an
operation of it is `absurd`). Scala 3 writes a row with type lambdas. Scala 2 has no type alias whose result is a
type constructor, so it writes one with names, the proper types `*[F]`, `F % S`, `R + E`, each with its constructor
as the member `L`; `*[F]` is `F` in Scala 3 too, so that code written alike for both compilers compiles. The shared
source writes a row as both read it, `({ type L[x] = Sum[R, E, x] })#L`, or by aliases, `type Desk[X] = Sum[...]`.

The evidence is found by implicit search, the same in both, from `splitOne`, `splitLast`, `splitInit` (`E` further
in). `handle` takes `E` from wherever it is in the row: by default `split(p)`, moving `E` to the end, then
`handleLast`, taking the last effect; an implementation may take it from its place at once. `p.into[S]` puts a
program in a row of more effects. `map` is a primitive: an implementation may do it in one step.

## Handlers

Five shapes, each a trait of clauses of no implementation (`Clauses`, `TailClauses`, …); `Handler[E, R, A, B]` is
`Clauses[E, A, B ! R]`. `handle(p, h)` and `handleLast(p, h)` take each shape, `handle(p, s0, h)` the stateful ones:

| | its clause gets `k` | answered in place, every clause resuming `k` once, at once | never resuming `k` |
|---|---|---|---|
| no state | `Handler` | `TailHandler` (`Reader`) | `AbortHandler` (`Error`) |
| a state of its own | `StatefulHandler`: the state of its time, `k(x, s)` resumed with the next | `StatefulTailHandler`, the state in a `Cell` (`State`, `Writer`) | |

A handler is deep: `k` is the rest of the program under the same handler, resumable any number of times. A stateful
general handler is a handler with a parameter: a `k` resumed twice runs each branch from its own state. The two
answered in place need no `k`: an implementation answers them where the operation finds them. An implementation
with no form of its own for a shape runs it as a general handler (the interface's defaults).

## An effect of one's own

An effect is a GADT of its operations, `F[S, X]` with a parameter, `F[X]` with none, the same type for both
compilers. In Scala 3 its operations may be an `enum`, its clauses typed by GADT inference:

```scala
enum Counter[X]:
  case Tick extends Counter[Unit]
  case Peek extends Counter[Int]

def tick[R[_]: Has[Counter]]: Unit ! R = perform[Counter](Counter.Tick)

def counting[R[_], Rest[_], A](p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
  handle(p, 0, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)]:
    def ret(n: Int, a: A) = pure((n, a))
    def answer[X](cell: Cell[Int], op: Counter[X]): X = op match
      case Counter.Tick => cell.set(cell.get + 1)
      case Counter.Peek => cell.get)
```

Scala 2 has no GADT inference for a method's type parameter, so each operation says how its answer is made,
`answer`, and a clause needs no cast:

```scala
sealed trait Counter[X]
object Counter {
  final case class Tick[X](answer: Unit => X) extends Counter[X]
  final case class Peek[X](answer: Int => X) extends Counter[X]
}

def tick[R[_]](implicit in: In[Counter, R]): Unit ! R = perform[Counter].apply[R, Unit](Counter.Tick(u => u))

def counting[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Counter, R, Rest]): (Int, A) ! Rest =
  handle[Counter, Int, R, Rest, A, (Int, A)](p, 0, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)] {
    def ret(n: Int, a: A) = pure((n, a))
    def answer[X](cell: Cell[Int], op: Counter[X]): X = op match {
      case Counter.Tick(answer) => cell.set(cell.get + 1); answer(())
      case Counter.Peek(answer) => answer(cell.get)
    }
  })
```

## The standard effects

`fx.std`, one source for both compilers, their definitions in the object `okay.effects.Std` (so that other effects of
the same names do not clash). Each handler takes its effect from anywhere in the row. `Std` is a class over the
implementation, made by `newStd`: an implementation may offer its own with better handlers.

| Effect | Operations | Handlers |
|---|---|---|
| `State % S` | `get`, `put`, `modify`, `gets(f)`, `getAndSet(s)` | `state(s0)`, in place |
| `Reader % E` | `ask` | `reader(e)`, in place; `local(f)(p)`: `p` sees `f(e)` |
| `Writer % W` | `tell` | `writer[W]`, in place; `listen[W](p)`, `censor(f)(p)`: what `p` told, told outside when it ends |
| `Error % E` | `raise` | `attempt[E]` (aborting), `recover[E](p)(h)` |
| `Choose` | `among`, `fail`, `guard(cond)` | `all` (every way, `k` resumed per choice), `first` (the first way with no `fail`) |
| `Yield % O` | `emit` | `collect[O]`, `take[O](n)`, `each[O](p)(f)` |
| `Abort` | `abort` | `optional(p)`: `None` if it aborted |
| `Fresh` | `fresh` | `freshFrom(n0)(p)` |
| `Random` | `nextLong`, `nextInt(bound)`, `nextDouble` | `random(seed)(p)`: the same seed, the same numbers |
| `Store[K, V, *]` | `load(k)`, `save(k, v)`, `remove(k)` | `store(m0)(p)`: the final map and the value |
| `Lift % G` | `lift(ga)`: a program of another monad `G` | a `Runner`'s `lower[G](p)`, given a `Lowering[G]` (cats, ZIO) |
| `Control % (B ! R)` | `shift(f)` | `reset(p)`: delimited control, the answer type fixed |

Delimited control CHANGING ANSWER TYPES is its own type, `Cps[A, B, C, R]` (`fx.delimited`, the trait `Delimited`):
`shift`, `reset`, `pure`, `map`, `flatMap`, `lift`. By default continuation-passing over any implementation, in
constant stack; okay-core's bridge has it native, on okay-core's own `shift0` and `$`.

## Interceptors

`intercept(p, h)`: `E`'s operations in `p` given to the handler `h` — with `k`, the rest, still intercepted — and the
row stays `R`, `E` in it. `h`'s clause runs outside the interception, so an operation of `E` it performs goes to `E`'s
handler further out: an operation observed and passed on, changed, refused, answered here. On it, in `std`:

```scala
observe[State % Int, R, A](p)(new Observe[State % Int, R] {          // each operation seen, then passed on
  def apply[X](op: State[Int, X]): Unit ! R = tell[String, R](op.toString)
})
around[Reader % Int, R, A](p)(new Around[Reader % Int, R] {          // each given with its way on, `proceed`
  def apply[X](op: Reader[Int, X], proceed: X ! R): X ! R = op match {
    case Std.Reader.Ask(answer) => ask[Int, R].map(e => answer(e * 2))
  }
})
```

The interface's primitive is `interposed(p)`, the implementation's: the program in `R + E`, its `E` moved to a new
last place, its other effects where they are — `intercept` is `handleLast` of it. The machine and `Free` do it by one
renaming of places (Scala 3: `Places.interposing`, typed), okay-kernel's bridge by its `remap`, okay-core's by its
observer (`Seen.at`, an operation's place), tagless on the implementation given.

## More effects

Modules over the interface, in `modules/`, one source for both compilers, each checked on the machine, on `Free` and on tagless; each a class
over an implementation, `Concurrent(fx)`, `Io(fx)`, ...:

| Module | What it gives |
|---|---|
| `concurrent` | cooperative threads: `pass`, a rendezvous channel (`send`, `recv`); `scheduled[V](threads)` runs them in turns, each a coroutine (its `Coop` handled to a `Step`, its rest resumed once); a thread waiting forever `None`. Structured: no `fork` from inside a thread — an effect carrying a program of its own row is of a type that contains itself |
| `resource` | `scoped(p)`, `defer(fin)`, `bracket(acquire)(release)(use)`: finalizers run at the scope's end, the last first; an error handled inside the scope ends it too |
| `io` | `Console`, `Clock`, `Env` as effects: the world's handlers (`realConsole`, `systemClock`, `systemEnv`) and pure ones for tests (`console(inputs)`, `clock(start, step)`, `env(vars)`) |
| `probability` | `flip`, `uniform`, `weighted`; `exact(p)` every outcome with its probability, `sampled(p)` one way by `Random` |
| `parsing` | parsers of `State[String]` and `Choose`: `item`, `satisfy`, `char`, `string`, `orElse`, `many`, `eof`; `parse(input)(p)` the first parse of all the input, backtracking |
| `logic` | terms, logic variables (`Fresh`), `unify` (bindings a `State`), `either`; `solutions(p)` every answer — a relation runs either way; `fair(n)(p)` a breadth-first search, so a way that never ends does not hide the others |
| `transaction` | `read`, `write` and `atomically(p)` on the standard `Store` outside: a log written at the end; an error handled outside drops it — rolled back |
| `resilience` | `retry[E](n)(p)`, `retryIf[E](n)(worth)(p)`, `fallback[E](p)(q)` |
| `stream` | `Stream[O, R]`: elements pulled one at a time, each pull a program of `R` — lazy, endless if need be; sources `emits`, `range`, `iterate`, `unfold`, `eval`, `repeatEval`; `map`, `evalMap`, `filter`, `take`, `drop`, `takeWhile`, `scan`, `zip`, `interleave`, `++`, `flatMap`; sinks `toList`, `fold`, `foreach`, `drain`; with the standard generators, `fromGenerator(p)` and `s.emitAll` |

Not here: a timeout (a runtime's: ZIO's, cats-effect's, through interop).

## Scala 2 and Scala 3: one interface, compatible

The interface is one source, `Effectful` (`effects/src/main/scala`): the program, the row's evidence, the
primitives, the handlers' shapes, `Cell`, the defaults. Each compiler's `Effects` (`scala-2/`, `scala-3/`) adds only
how a row is written, and the program's methods (Scala 3's inline extensions, Scala 2's implicit class). So the
standard effects, the protocol's bridge, the interop and the suite's checks are one source too; the machines and
`Free` are each compiler's, as Scala 2 has no GADT inference to type the walk to a handler.

Compatible means: one application has one build of the interface, and code of the other compiler takes it as it is.
An application with Scala 2 code in it takes the Scala 2.13 build, which Scala 3 reads: its Scala 3 code writes rows
as Scala 2 does. An application of Scala 3 alone takes the Scala 3 build. Proven by:

| Module | What it shows |
|---|---|
| `suite`, `suite-free`, both compilers | the interface's suite (`tck`) on each implementation |
| `compat/scala3-on-2.13` | Scala 2's programs and suite compiled by Scala 3 against the build Scala 2.13 made |
| `compat/scala2`, `compat/scala3` | an effect, its handler and a program written in Scala 2.13, used by Scala 3 code beside its own |
| `examples/mixed` | the same as an example, on the machine and on `Free` |

## Implementations

- **The machine** (`core/machine/`), one per compiler, `okay.effects.machine.Machine`; its classes `Engine`, in an
  object — no outer pointer in a node or a level. A program is data, `Prog[R, A]`; the run a `@tailrec` loop over it,
  its FRAMES (`Frames[R, X, V]`, the binds from `X` to the value `V` of the nearest level's body, aligned) and its
  LEVELS (`Meta[R, V, Z]`, a handler of `E` at `T + E`, a remapping of places). An answer in place goes straight into
  the next frame; a `map` is one step; a `k` taken is data, the levels crossed a `Prefix` put back where it is
  resumed. The loop holds the hot cases, `cold` the rest. No exception: an operation past every handler is of
  `Void`, absurd.
  - **Scala 3's**: its data enums, typed by GADT inference throughout, NO CAST. A place is a typed path
    (`Places.Place`: `Last`, `Init`, `Only`, `Absurd` — the `Split` evidence itself); a path matched against a
    handler's level is, by the types, that handler's operation.
  - **Scala 2's**: a place a number, the walk to a handler over the levels' fields (`kind`, `outer`) with no call;
    each level does for itself what needs its types (`returned`, `stepOut`, `found`), as a match refines none. Three
    trusted steps, named: an operation at its handler of its effect, an answer in place of its type, `split` at the
    last place no move.
- **The `Free` monad** (`core/free/`), one per compiler: `Free[F, A]` — `Return`, `Perform`, `Bind`, `Inject` (an
  operation and its rest, one node), `Delay` — covariant in its value; `F` a row's operations, each carrying its
  place (`At`). A handler interprets the tree (`Free.handle`, `Free.answer` in place, each with or without a state),
  an operation passed on by each handler it crosses, one place nearer; `k` a trampoline. A handler takes its effect
  from its place in the row at once (`handleAt`, `answerAt`; Scala 3): no `split`, no program renamed. Scala 3's typed by
  `Places`, no cast; Scala 2's places numbers, one trusted step.
- **okay-core's bridge** (okay-core's `effects/`): a program okay-core's own `Cont`, nothing wrapped; a place okay-core's
  path, `Split` its `Removed`; a handler one of okay-core's forms (`Deep`, `Answering`, `Stateful`), installed by its
  `handle` at the `Removed`; `Delimited` native.
- **okay-kernel's bridge** (okay-kernel's `effects/`): on any implementation of the protocol — its reference machine,
  okay-java, okay-core's bridge. Every operation of one key carrying its place, each handler a handler of that key
  answering place 0 and passing any other on, one place in: two effects of one type in a row stay two places.

All run in constant stack: 100 000 operations under three handlers, 100 000 binds.

## Tagless

`tagless/`: `Tagless`, an `Effects` of NO implementation — the same words, `!`, `In`, `Split`, `perform`, `handle`
of each shape, `std`. A program is a `Program[A, R]`, a function of the implementation, `on(fx)`; so is the evidence.
The program is a value, made once and run on any implementation; the implementation is asked for once, at `run`, a
`Runner` from the implicit context:

```scala
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.tagless.Tagless.*
import okay.effects.tagless.Tagless.std.*

def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R =
  for { name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + 1) } yield n

val greeted: (Int, Int) ! Pure = state(0)(reader("Ann")(greet[Reader % String + State % Int]))

given Runner = Machine      // or Free, or okay-core's Core
run(greeted)
```

Being an `Effects`, it runs code written over the interface: `Std`, `Delimited` (by continuations), anything taking
`fx: Effects`. Each word made on `fx` is `fx`'s own: `pure` its `pure`, a handler its `handle` of the same shape —
so what an implementation does better it does here too. One trusted step: the `k` an implementation gives a general
handler's clause is given back to the clause as a `Program` (a cast, checked by identity: a `k` kept and run on
another implementation fails).

`Tagless.on(fx)`, a `Runner`: tagless run on `fx`, for code that runs programs over the interface — the interface's
suite runs on it, on the machine and on `Free`. One per compiler; Scala 3's every word `inline`. A chain of binds is
made by nested calls 64 deep at most, then `delay`ed: constant stack, one `delay` in 64.

Its price, a `Program` made and then made again on `fx` at each node (Scala 3, µs, the programs of `Measured`):

| Program | machine | tagless on the machine | `Free` | tagless on `Free` |
|---|---|---|---|---|
| 1000 maps | 6.9 | 10.1 | 12.0 | 16.1 |
| 1000 `ask`s under their handler | 17.2 | 23.9 | 33.9 | 40.4 |
| across four handlers | 23.3 | 39.7 | 71.0 | 89.9 |
| 1000 `get`/`put` pairs | 18.5 | 31.3 | 20.5 | 39.7 |

## Interop

Over any implementation, both compilers; another monad's programs inside a program are the standard `Lift % G`
(`std.lift(ga)`), and a program of it given back as a `G[A]` by a `Runner`'s `lower(p)`, given a `Lowering[G]`:

| Module | What it gives |
|---|---|
| `interop/cats` | `Cats(fx).monad[R]`, cats' `Monad` of the programs (stack safe); a `Lowering` of any cats monad — `IO` of cats-effect 3, `Either`, `Option` |
| `interop/zio` | `ZioEffects(fx)`: `fromZIO(task)`, `toZIO(p)` — a ZIO task a `Lift % Task`, a program a `Task` |
| `interop/tofu` | `Tofu(Cats(fx))`: `WithLocal` of a `Reader`, `Errors` of an `Error`, anywhere in a row named |

## Examples

`examples/`, written over `Effects` alone and run on each implementation, `OnMachine` and `OnFree`:

| File | What it shows |
|---|---|
| `scala/.../Shared.scala` | code written once for both compilers: an effect, its handler, a row by aliases |
| `scala-3/.../Basics3.scala` | Scala 3's rows and context bounds, an effect of one's own an `enum`, a handler in place, a general one resuming `k` twice |
| `scala-3/.../Control3.scala` | generators (`collect`, `take`, `each`), a search (`first`), `recover`, `shift`/`reset`, the answer type changed |
| `scala-2/.../Basics2.scala` | Scala 2's rows by names (`#L`), an effect's operations with their answers, a search |
| `mixed/.../Mixed.scala` | one application of two compilers: Scala 3 code using Scala 2.13's effect, handler and program, one row, one run |

```sh
sbt "examples/runMain okay.effects.examples.OnMachine"   # the examples on the machine
sbt "examples/runMain okay.effects.examples.OnFree"      # on the Free monad
```

## Measured

`bench/`, both compilers (JMH `-f 2 -wi 5 -i 5 -w 1 -r 1`, JDK 26; `bench.d/`), one run of each program, n = 1000,
µs. Each program twice where it says so: at a row named in it, and row-polymorphic (`…In`: its effects asked as `In`
evidence, passed on to the recursion):

| Program | machine, Scala 3 | machine, Scala 2 | `Free`, Scala 3 | `Free`, Scala 2 |
|---|---|---|---|---|
| 1000 maps, no operation | 6.9 | 6.9 | 11.9 | 11.8 |
| 1000 `ask`s under their handler, a map pending each | 17.3 | 20.8 | 34.0 | 32.1 |
| the same, row-polymorphic | 17.3 | 21.6 | 31.5 | 31.9 |
| across four handlers of other effects | 23.6 | 17.2 | 70.7 | 70.4 |
| the same, row-polymorphic | 19.0 | 16.7 | 64.6 | 68.5 |
| 1000 `get`/`put` pairs under `State` | 18.6 | 16.3 | 20.6 | 23.6 |
| the same, row-polymorphic | 17.6 | 16.6 | 18.7 | 22.4 |

- Scala 3's places are typed paths, made where a row is named and matched by type; Scala 2's are numbers, walked by
  the levels' fields — across handlers Scala 2's machine is the faster, its trusted steps the price.
- `Free` interprets the tree by each handler in turn, so an operation across handlers costs per handler. A handler
  taking its effect from its place, with no remap of the program, made `State` under Scala 3's `Free` 31.2 → 20.6.
- okay-core's machine and the protocol's implementations are measured in their repositories, beside their bridges.

## Build

```sh
sbt testAll                                      # both compilers: everything here
sbt "+effects/publishLocal" "+tck/publishLocal"  # the interface and its suite, for okay-core's and okay-kernel's bridges
python3 /tmp/okz/tools/withlock.py sbt "bench/Jmh/run -f 2 -wi 5 -i 5 -w 1 -r 1 -prof gc"   # JMH, holding JMH's lock
```
