# okay-simple

One simple interface for effect systems, for Scala 3 and Scala 2.13: the types `!`, `+`, `%`, `*`, the functions
`perform`, `handle`, `split`, `run` (module `api`, package `okay.api`), and the standard effects written once on them
(module `std`, package `okay.std`; their definitions in the object `okay.std.Std`, so that other effects of the same
names in `okay.std` do not clash). The interface fixes no implementation: a program, a row and the evidence that an
effect is in a row are abstract types, and an implementation makes them what it likes. `simple/` is its own
implementation; `core/` runs the same programs on okay-core's machine, `kernel/` on okay-kernel's protocol. An
implementation knows nothing of the standard effects: they are the trait `Std`, mixed in where an implementation is
offered (`Simple`, `Core`, `OnKernel.on`).

## A program

```scala
import okay.simple.machine.Simple._          // everything: the types, the primitives, the effects and their handlers

// in any row that has these three, at any place
def greet[R](implicit r: In[Reader % String, R], w: In[Writer % String, R], s: In[State % Int, R]): Int ! R =
  for {
    name <- ask[String, R]
    _    <- tell[String, R](s"hello, $name")
    n    <- get[Int, R]
    _    <- put[Int, R](n + name.length)
  } yield n

type Row = (Reader % String) + (Writer % String) + (State % Int)    // Scala 2: each % in parentheses
run(reader("Ann")(state(10)(writer[String](greet[Row]))))             // (13, (List("hello, Ann"), 10))
```

In Scala 3 the row needs no parentheses, `Reader % String + Writer % String + State % Int`, and the requirements
read as context bounds: `def greet[R: {Has[Reader % String]#L, Has[State % Int]#L}]: Int ! R`. `Has[E]#L` works in
Scala 2 too.

## The types

| | |
|---|---|
| `A ! R` | a program of value `A` over the row `R` |
| `Pure`, `E`, `R + E` | a row: of no effect, of one, the row `R` and `E` after it — left to right |
| `F % S` | the effect of the operations `F[S, X]`, with its parameter `S`: `State % Int`, its operations `State[Int, X]` |
| `*[F]` | the effect of the operations `F[X]`, with no parameter: `*[Choose]`, its operations `Choose[X]` |
| `Effect`, `Name[F]` | what `%` and `*` are: an effect's name, a proper type, and the operations it names |
| `Op[E, X]` | the operation of `E` answered by an `X`: computed from the name, the same wherever it is asked |
| `In[E, R]` | `E` is in `R` |
| `Split[E, R, Rest]` | `E` is in `R`, at any place, and `R` without it is `Rest` |
| `Sub[R, S]` | every effect of `R` is in `S` |

The evidence is found by implicit search, the same in both compilers, from `splitOne`, `splitLast`, `splitInit`
(`E` further in). `handle` takes `E` from wherever it is in the row: by default `split(p)`, moving `E` to the end
of the row, then `handleLast`, taking the last effect — an implementation may take it from its place at once. The
same for the two other shapes of handler, `handleTail` and `handleStateful` (`handleTailLast`, `handleStatefulLast`).
`p.into[S]` puts a program in a row of more effects.

## An effect and its handler

An effect is a GADT of its operations — `F[S, X]` with a parameter, `F[X]` with none — and in a row it is named
`F % S` or `*[F]`: a row holds proper types, and Scala 2 has no type alias whose result is a type constructor. A
handler is typed by the name, its clause by `Op[E, X]` — so an operation and the handler it goes to are of one type,
computed from the name on both sides. Each operation says how its answer is made, `answer`, so a handler's clause needs no
GADT inference (Scala 2 has none for a method's type parameter) and no cast:

```scala
sealed trait Counter[X]                                                     // its operations; named *[Counter]
object Counter {
  final case class Tick[X](answer: Unit => X) extends Counter[X]
  final case class Peek[X](answer: Int => X) extends Counter[X]
}

def tick[R](implicit in: In[*[Counter], R]): Unit ! R = perform[*[Counter]].apply[R, Unit](Counter.Tick(u => u))

def counting[R, Rest, A](p: A ! R)(implicit s: Split[*[Counter], R, Rest]): (Int, A) ! Rest =
  handle[*[Counter], R, Rest, A, Int => (Int, A) ! Rest](p)(new Handler[*[Counter], Rest, A, Int => (Int, A) ! Rest] {
    def ret(a: A) = pure(n => pure((n, a)))
    def op[X](op: Op[*[Counter], X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match {
      case Counter.Tick(answer) => pure(n => k(answer(())).flatMap(f => f(n + 1)))
      case Counter.Peek(answer) => pure(n => k(answer(n)).flatMap(f => f(n)))
    }
  }).flatMap(f => f(0))
```

A handler is deep: `k` is the rest of the program under the same handler, and it can be resumed any number of
times (`Choose`'s `all` resumes it once per choice). A handler of an effect with a parameter is typed by its name,
`Handler[State % S, R, A, B]`, its clause by `Op[State % S, X]`, which is `State[S, X]`. In Scala 3, an effect's
operations may be an `enum` with no `answer`, its clauses typed by GADT inference.

The standard effects, each handled from anywhere in the row: `State` (`state(s0)`), `Reader` (`reader(e)`),
`Writer` (`writer[W]`), `Error` (`attempt[E]`), `*[Choose]` (`all`).

## Scala 2 and Scala 3: two conveniences, one interface

Why the row names effects by proper types: Scala 2 has no type alias whose result is a type constructor, so `%` and
`+` cannot make an `F[_]` there. A name is what both compilers share. `Op[E, X]` is each compiler's own: in Scala 2
the name's member, `E#Ops[X]` (Scala 2 selects a type from an abstract one); in Scala 3 a match type on the name
(Scala 3 selects none) — `api/src/main/scala-2`, `scala-3`.

Compatible means: what code of one compiler makes, code of the other takes as it is — an effect, a handler, a
program, a row type. One application has one build of the interface. A Scala 3 build's `Op` is a match type, which
Scala 2 cannot read; so an application with Scala 2 code in it takes the Scala 2.13 build, which Scala 3 reads.
Everything else is shared under the same names (`Has[E]#L` for a context bound in both). Proven by:

| Module | What it shows |
|---|---|
| `suite` test, both compilers | the interface's suite, from one source, on each implementation |
| `compat/scala3-on-2.13` | the interface's suite compiled by Scala 3 against the build Scala 2.13 made |
| `compat/scala2` | an effect, its handler and a program written in Scala 2.13, on the Scala 2.13 build |
| `compat/scala3` | Scala 3 code using that Scala 2.13 library beside its own, in one row, one run |

## Implementations

All behind the same interface, all run by the same suites (`suite/`; `machine` and the protocol's bridge by both
compilers).

- **`simple/`, the machine, one per compiler** — the same interface, the same suites, `okay.simple.machine.Simple`:
  - **Scala 3's own** (`simple/src/main/scala-3`): typed by Scala 3's GADT inference throughout, NO CAST. A program
    is `Prog[R, A]`, of its row; an operation `Perf(at, op)`, its place a typed path (`LastPlace`, `InitPlace`,
    `OnePlace` — the `Split` evidence itself), its operation `Op[E, X]`. The frames `Frames[R, X, V]` aligned, the
    levels `Meta[R, V, Z]` indexed by the row; a path matched against a handler's level is, by the types, that
    handler's operation. The machine's data and loop live in an object, static: no outer pointer in a node, no
    outer test in a match (in a class they cost 8 B an object and a test a step); a handler is kept as the
    object's own shape of it, wrapped once where it is installed.
  - **Scala 2's** (`simple/src/main/scala-2`): the same design where Scala 2 can follow it — Scala 2 has no GADT
    inference, so a place is a shared number, three trusted steps where an operation meets its handler, and
    methods each level means for itself (`last`, `top`, `returned`, `stepOut`) where Scala 3 matches.
- **`core/`, on okay-core's machine** (Scala 3), by okay-core's public API only: a program is okay-core's own
  `Cont`, nothing wrapped. An okay-simple place is okay-core's path (the last effect of an okay-simple row is the
  head of okay-core's), `Split` is okay-core's `Removed`, made of the place; a general handler is a `Deep`, one
  answering in place an `Answering`, a stateful one okay-core's `Stateful`, installed by okay-core's `handle` at the
  `Removed` — the effect taken from its place at once (the interface's `handle`, `handleTail`,
  `handleStateful`, which the standard handlers use). A `split` on its own is `widen` and a handler translating
  the effect to the fresh head; an `embed` a handler translating each effect to its place. Built against okay-core's
  artifact (`okay-core::okay-core:0.1.0-SNAPSHOT`, published from okay-core's master: `sbt coreJVM/publishLocal`).

- **`kernel/`, on okay-kernel's protocol** (both compilers): on any implementation of it, given its `Effects` —
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

Three shapes of handler: a general one, `Handler`, whose clause gets `k`; one answering in place, `TailHandler`, every
clause resuming `k` once, at once (`Reader`); and the same with a state of its own in a `Cell`, `StatefulHandler`
(`State`, `Writer`). The machine answers the last two where the operation finds them: no `k`, no allocation, the
state written in its level. A `k` taken by a general handler across a stateful level copies its state, so resumed
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
