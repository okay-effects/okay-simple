# okay-simple

One simple interface for effect systems, for Scala 3 and Scala 2.13: the types `!`, `+`, `%`, the functions
`perform`, `handle`, `split`, `run`, and the standard effects written once on them, `Std` (module `effects`, package
`okay.effects`; the standard effects' definitions inside the object `Std`, so that other effects of the same names
do not clash). The interface is ONE for both compilers, `Effectful`: a row is a type
constructor in both, and everything over it is one source. Only how a row is written is each compiler's own
`Effects`: in Scala 3 by type lambdas, `Choose + State % Int`; in Scala 2 by names taken as the constructor they
make, `(*[Choose] + (State % Int))#L`. The interface fixes no implementation: a program, a row's evidence
are abstract types, and an implementation makes them what it likes. `machine/` is its own implementation; `core/`
runs the same programs on okay-core's machine, `kernel/` on okay-kernel's protocol. An implementation knows nothing
of the standard effects: they are the trait `Std`, mixed in where an implementation is offered (`Machine`, `Core`,
`OnKernel.on`).

## A program

Scala 3:

```scala
import okay.effects.simple.Machine.*          // everything: the types, the primitives, the effects and their handlers

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
import okay.effects.simple.Machine._

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
| `E` is in `R`, at any place, and `R` without it is `Rest` | `Split[E, R, Rest]` | the same |
| every effect of `R` is in `S` | `Sub[R, S]` | the same |

A row is a type constructor: `R + E` is the phantom `Sum[R, E, X]`, `Pure` the empty `Void`, and an effect is in
a row as it is. Scala 3 writes it with type lambdas. Scala 2 has no type alias whose result is a type constructor,
so it writes a row with names, the proper types `*[F]`, `F % S`, `R + E`, each with its constructor as the member
`L`; `*[F]` is `F` in Scala 3 too, so that code written alike for both compilers compiles. The shared source writes
a row as both read it, `({ type L[x] = Sum[R, E, x] })#L`.

The evidence is found by implicit search, the same in both, from `splitOne`, `splitLast`, `splitInit` (`E`
further in). `handle` takes `E` from wherever it is in the row: by default `split(p)`, moving `E` to the end of the
row, then `handleLast`, taking the last effect; an implementation may take it from its place at once. Both take a
handler of any of the four shapes: `handle(p, h)`, `handle(p, s0, h)` for a stateful one.
`p.into[S]` puts a program in a row of more effects.

## An effect and its handler

An effect is a GADT of its operations, `F[S, X]` with a parameter, `F[X]` with none, the same type for both
compilers. In Scala 3 its operations may be an `enum`, its clauses typed by GADT inference:

```scala
enum Counter[X]:
  case Tick extends Counter[Unit]
  case Peek extends Counter[Int]

def tick[R[_]: Has[Counter]]: Unit ! R = perform[Counter](Counter.Tick)

def counting[R[_], Rest[_], A](p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
  handle[Counter, R, Rest, A, Int => (Int, A) ! Rest](p, new Handler[Counter, Rest, A, Int => (Int, A) ! Rest]:
    def ret(a: A) = pure(n => pure((n, a)))
    def op[X](op: Counter[X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match
      case Counter.Tick => pure(n => k(()).flatMap(f => f(n + 1)))
      case Counter.Peek => pure(n => k(n).flatMap(f => f(n)))
  ).flatMap(f => f(0))
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
  handle[Counter, R, Rest, A, Int => (Int, A) ! Rest](p, new Handler[Counter, Rest, A, Int => (Int, A) ! Rest] {
    def ret(a: A) = pure(n => pure((n, a)))
    def op[X](op: Counter[X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match {
      case Counter.Tick(answer) => pure(n => k(answer(())).flatMap(f => f(n + 1)))
      case Counter.Peek(answer) => pure(n => k(answer(n)).flatMap(f => f(n)))
    }
  }).flatMap(f => f(0))
```

A handler is deep: `k` is the rest of the program under the same handler, and it can be resumed any number of
times (`Choose`'s `all` resumes it once per choice).

The standard effects, each handled from anywhere in the row: `State` (`state(s0)`), `Reader` (`reader(e)`),
`Writer` (`writer[W]`), `Error` (`attempt[E]`), `Choose` (`all`): one source for both compilers, their
definitions the object `okay.effects.Std`, their operations and handlers the trait `Std`.

## Scala 2 and Scala 3: one interface, compatible

The interface is one source, `Effectful` (`effects/src/main/scala`): the program, the row's evidence, the primitives,
the handlers' shapes (`Clauses`, `TailClauses`, `StatefulClauses`, `StatefulTailClauses`; `Handler[E, R, A, B]` is
`Clauses[E, A, B ! R]`),
`Cell`, the defaults. Each compiler's `Effects` (`scala-2/`, `scala-3/`) adds only how a row is written. So the
standard effects, the protocol's bridge and the suite's checks are one source too; the machines are each
compiler's, as Scala 2 has no GADT inference to type the walk to a handler.

Compatible means: one application has one build of the interface, and code of the other compiler takes it as it
is. An application with Scala 2 code in it takes the Scala 2.13 build, which Scala 3 reads: its Scala 3 code
writes rows as Scala 2 does. An application of Scala 3 alone takes the Scala 3 build. Proven by:

| Module | What it shows |
|---|---|
| `suite` test, both compilers | the interface's suite on each implementation: its checks shared, its programs each compiler's rows |
| `compat/scala3-on-2.13` | Scala 2's programs and suite compiled by Scala 3 against the build Scala 2.13 made |
| `compat/scala2` | an effect, its handler and a program written in Scala 2.13, on the Scala 2.13 build |
| `compat/scala3` | Scala 3 code using that Scala 2.13 library beside its own, in one row, one run |

## Implementations

All behind the same interface, all run by the same suites (`suite/`; `machine` and the protocol's bridge by both
compilers).

- **`machine/`, the machine, one per compiler** — the same interface, the same suites, `okay.effects.simple.Machine`:
  - **Scala 3's own** (`machine/src/main/scala-3`): typed by Scala 3's GADT inference throughout, NO CAST. A program
    is `Prog[R, A]`, of its row, a type constructor; an operation `Perf(at, op)`, its place a typed path
    (`LastPlace`, `InitPlace`, `OnePlace` — the `Split` evidence itself), its operation an `E[X]`. The frames `Frames[R, X, V]` aligned, the
    levels `Meta[R, V, Z]` indexed by the row; a path matched against a handler's level is, by the types, that
    handler's operation. The machine's data and loop live in an object, static: no outer pointer in a node, no
    outer test in a match (in a class they cost 8 B an object and a test a step); a handler is kept as it is
    given, the shared `Clauses`, no wrapper.
  - **Scala 2's** (`machine/src/main/scala-2`): the same design where Scala 2 can follow it — Scala 2 has no GADT
    inference, so a place is a shared number, three trusted steps where an operation meets its handler, and
    methods each level means for itself (`last`, `top`, `returned`, `stepOut`) where Scala 3 matches.
- **`core/`, on okay-core's machine** (Scala 3), by okay-core's public API only: a program is okay-core's own
  `Cont`, nothing wrapped. An okay-simple place is okay-core's path (the last effect of an okay-simple row is the
  head of okay-core's), `Split` is okay-core's `Removed`, made of the place; a general handler is a `Deep`, one
  answering in place an `Answering`, a stateful one okay-core's `Stateful`, installed by okay-core's `handle` at the
  `Removed` — the effect taken from its place at once (the interface's `handle`, of each shape, which the
  standard handlers use). A `split` on its own is `widen` and a handler translating
  the effect to the fresh head; an `embed` a handler translating each effect to its place. Built against okay-core's
  artifact (`okay-core::okay-core:0.1.0-SNAPSHOT`, published from okay-core's master: `sbt coreJVM/publishLocal`).

- **`kernel/`, on okay-kernel's protocol** (both compilers, one source: the protocol's part `Protocol`, the
  interface over it `OnKernel`): on any implementation of it, given its `Effects` —
  okay-kernel's reference machine (`Steps`), okay-java (`okay.core.Kernel.EFFECTS`), okay-core's bridge
  (`okay.core.kernel.OnCore`): `OnKernel.on(effects)`. The protocol finds a handler by an operation's key, okay-simple
  by its place; the bridge keeps okay-simple's meaning exactly: every okay-simple operation is of one key and carries
  its place, each okay-simple handler is a handler of that key answering place 0 and passing any other on, one place
  in. So two effects of one type in a row stay two places. A general handler is the protocol's `General`, one
  answering in place its `Translating` (no `k`), a stateful one the interface's default, a remapping a `Translating`.
  The price of the exactness: an operation is performed again at each okay-simple handler it crosses.

All of them share a place's evidence, one object per depth: an operation allocates nothing for its place, and
okay-core's look-up, kept on its path, is found again the next time.

Both run in constant stack: 100 000 operations under three handlers, 100 000 binds.

Four shapes of handler, general or answering in place, with a state of its own or without:

| | its clause gets `k` | answered in place, every clause resuming `k` once, at once |
|---|---|---|
| no state | `Handler` | `TailHandler` (`Reader`) |
| a state of its own | `StatefulHandler`: the state of its time, `k` resumed with the next | `StatefulTailHandler`, the state in a `Cell` (`State`, `Writer`) |

A stateful general handler is a handler with a parameter: `k(x, s)` goes on from the state `s`, so a `k` resumed
twice runs each branch from its own. The machine answers the two in place where the operation finds them: no `k`,
no allocation, the state written in its level. A `k` taken by a general handler across a stateful level copies its state, so resumed
twice it resumes the state of its time. An implementation with no such forms runs them as general handlers (the
interface's defaults).

Measured (`bench/`, JMH `-f 2 -wi 3 -i 5 -prof gc`, JDK 26, Scala 3.9; `bench.d/2026-10-17_all-typed.csv`; earlier
runs beside it), one run of each program, n = 1000, µs and allocation. Each program twice: at a row named in it, and
row-polymorphic (`…In`: its effects asked as `In` evidence, passed on to the recursion — the places made once):

| Program | `machine` | `core` | protocol: `Steps` | protocol: okay-java | protocol: okay-core |
|---|---|---|---|---|---|
| 1000 maps, no operation | 11.7, 142 KB | 10.6, 126 KB | 20.5, 230 KB | 18.0, 166 KB | 17.0, 182 KB |
| 1000 `ask`s under their handler, a map pending each | 23.2, 303 KB | 19.0, 222 KB | 5428, 24.6 MB | 42.5, 471 KB | 67.0, 630 KB |
| the same, row-polymorphic | 25.7, 279 KB | 20.1, 222 KB | 5382, 24.6 MB | 44.0, 471 KB | 68.1, 630 KB |
| across four handlers of other effects | 32.9, 400 KB | 16.0, 207 KB | 5093, 26.1 MB | 114, 1.10 MB | 158, 1.50 MB |
| the same, row-polymorphic | 28.5, 280 KB | 19.1, 231 KB | 5103, 26.1 MB | 113, 1.10 MB | 287, 1.50 MB |
| 1000 `get`/`put` pairs under `State` | 23.3, 319 KB | 16.1, 214 KB | 138, 1.38 MB | 70.6, 863 KB | 102, 1.08 MB |
| the same, row-polymorphic | 22.9, 279 KB | 16.6, 222 KB | 140, 1.38 MB | 71.8, 871 KB | 103, 1.08 MB |

- `machine` is typed: in Scala 3 a place is a typed path, made at each operation where the row is named in the
  program — row-polymorphic code makes it once (400 → 280 KB across four handlers). Before the typed places, with a
  number and a cast, it was 18.9 / 26.4 / 17.4 µs (`bench.d/2026-10-17_machine-typed-final.csv`).
- okay-core's machine is the fastest: an operation crossing handlers is found at once, its look-up kept on its path.
- The protocol's reference machine, `Steps`, is quadratic when maps are pending: each operation's `k` is rebuilt
  from every frame on the stack. Through the protocol an operation is performed again at each okay-simple handler it
  crosses: across four handlers 2.7 times the cost under its own (okay-java 42 → 114 µs).

## Build

```sh
sbt testAll        # both compilers, okay-core's machine, okay-kernel's protocol on three implementations, and the three compatibility checks
sbt suite/test     # Scala 3
sbt "++2.13.18 suite/test"
python3 /tmp/okz/tools/withlock.py sbt "bench/Jmh/run -f 2 -wi 3 -i 5 -prof gc"   # JMH, holding JMH's lock
```
