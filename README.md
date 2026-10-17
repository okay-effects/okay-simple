# okay-simple

One simple interface for effect systems, for Scala 3 and Scala 2.13: the types `!`, `+`, `%`, the functions
`perform`, `handle`, `split`, `run`, and the standard effects written once on them. The interface fixes no
implementation: a program, a row and the evidence that an effect is in a row are abstract types, and an
implementation makes them what it likes. `free/` is the reference implementation, `machine/` the fast one, and
`okaycore/` runs the same programs on okay-core's machine.

## A program

```scala
import okay.simple._
val fx: Simple = okay.simple.machine.MachineEffects.effects
import fx._

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
| `F % S` | the effect `F` with its parameter `S`: `State % Int`, its operations `State[Int, X]` |
| `In[E, R]` | `E` is in `R` |
| `Split[E, R, Rest]` | `E` is in `R`, at any place, and `R` without it is `Rest` |
| `Sub[R, S]` | every effect of `R` is in `S` |

The evidence is found by implicit search, the same in both compilers, from `splitOne`, `splitLast`, `splitInit`
(`E` further in). `split(p)` moves `E` to the end of the row, wherever it was; `handle` takes the last effect;
`handleAny` is the two; `p.into[S]` puts a program in a row of more effects.

## An effect and its handler

An effect is a GADT of its operations, `F[X]` an operation answered by an `X`. Each operation says how its answer is
made, `answer`, so a handler's clause needs no GADT inference — Scala 2 has none for a method's type parameter —
and no cast:

```scala
sealed trait Counter
object Counter {
  sealed trait Op[X]
  final case class Tick[X](answer: Unit => X) extends Op[X]
  final case class Peek[X](answer: Int => X) extends Op[X]
  implicit val ops: Ops.Aux[Counter, Op] = Ops.of[Counter, Op]          // the name and its operations
}

def tick[R](implicit in: In[Counter, R]): Unit ! R = perform[Counter].apply[R, Unit](Counter.Tick(u => u))

def counting[R, Rest, A](p: A ! R)(implicit s: Split[Counter, R, Rest]): (Int, A) ! Rest =
  handleAny[Counter, Counter.Op, R, Rest, A, Int => (Int, A) ! Rest](p)(new Handler[Counter.Op, Rest, A, Int => (Int, A) ! Rest] {
    def ret(a: A) = pure(n => pure((n, a)))
    def op[X](op: Counter.Op[X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match {
      case Counter.Tick(answer) => pure(n => k(answer(())).flatMap(f => f(n + 1)))
      case Counter.Peek(answer) => pure(n => k(answer(n)).flatMap(f => f(n)))
    }
  }).flatMap(f => f(0))
```

A handler is deep: `k` is the rest of the program under the same handler, and it can be resumed any number of
times (`Choose`'s `all` resumes it once per choice). An effect with a parameter has one `Ops` for all, `F % S`; its
handler is a `HandlerOf[F, S, R, A, B]`, its operations `F[S, X]` with no type lambda to write. In Scala 3, an
effect may be an `enum` with no `answer`, its clauses typed by GADT inference, and a handler's operations may be
written `State Of Int`.

The standard effects, each handled from anywhere in the row: `State` (`state(s0)`), `Reader` (`reader(e)`),
`Writer` (`writer[W]`), `Error` (`attempt[E]`), `Choose` (`all`).

## Scala 2 and Scala 3: two conveniences, one interface

Why the row names effects by proper types: Scala 2 has no type alias whose result is a type constructor, so `%` and
`+` cannot make an `F[_]` there. A name is what both compilers share, and `Ops` ties it to its operations. Scala 3
alone could do with rows of higher kind; the interface does not, so that the two are one.

Compatible means: what code of one compiler makes, code of the other takes as it is — an effect, a handler, a
program, a row type. One application has one build of the interface, made by one compiler. So everything Scala 2
code needs lives in the source both compilers build, under the same names (`Has[E]#L`, `HandlerOf`); each
compiler's `Syntax` (`core/src/main/scala-2`, `scala-3`) only adds what the other does not need. Proven by:

| Module | What it shows |
|---|---|
| `suite` test, both compilers | the interface's suite, from one source, on each implementation |
| `compat/scala2` | an effect, its handler and a program written in Scala 2.13, compiled against Scala 3's build (`-Ytasty-reader`) |
| `compat/scala3` | Scala 3 code using that Scala 2.13 library beside its own, in one row, one run |
| `compat/scala3-on-2.13` | the interface's suite compiled by Scala 3 against the build Scala 2.13 made |

## Three implementations

All behind the same interface, all run by the same suites (`suite/`; `free` and `machine` by both compilers).

- **`free/`, the reference**: a free monad. A row is an ordered list and an operation carries its place in it,
  counted from the end. `handle` answers place 0, and passes any other operation on: it makes a new node of it, one
  place in, and wraps the rest in itself — so an operation costs a node and a closure for every handler it crosses.
  Short, and easy to check.
- **`machine/`, the fast one**: a machine of an explicit stack — the frames of the binds over the nearest level, and
  the levels outside it, each a handler or a remapping of places (`split`, `embed`) with the frames outside it. An
  operation walks the levels outward: a remapping renames its place, a handler not its own takes one off. Nothing is
  allocated on the way and nothing is passed on. At its handler the rest up to it, the handler included, is `k`: the
  frames inside and the levels crossed, immutable, so `k` is resumed any number of times; resumed, they are put back
  on the stack where the resumption is.

- **`okaycore/`, on okay-core's machine** (Scala 3), by okay-core's public API only: a program is okay-core's own
  `Cont`, nothing wrapped. An okay-simple place is okay-core's path (the last effect of an okay-simple row is the
  head of okay-core's), `Split` is okay-core's `Removed`, made of the place; a general handler is a `Deep`, one
  answering in place an `Answering`, a stateful one okay-core's `Stateful`, installed by okay-core's `handle` at the
  `Removed` — the effect taken from its place at once (the interface's `handleAny`, `handleTailAny`,
  `handleStatefulAny`, which the standard handlers use). A `split` on its own is `widen` and a handler translating
  the effect to the fresh head; an `embed` a handler translating each effect to its place. Built against okay-core's
  artifact (`okay-core::okay-core:0.1.0-SNAPSHOT`, published from okay-core's master: `sbt coreJVM/publishLocal`).

Both run in constant stack: 100 000 operations under three handlers, 100 000 binds.

Three shapes of handler: a general one, `Handler`, whose clause gets `k`; one answering in place, `TailHandler`, every
clause resuming `k` once, at once (`Reader`); and the same with a state of its own in a `Cell`, `StatefulHandler`
(`State`, `Writer`). The machine answers the last two where the operation finds them: no `k`, no allocation, the
state written in its level. A `k` taken by a general handler across a stateful level copies its state, so resumed
twice it resumes the state of its time. `free` runs them as general handlers (the interface's defaults).

Measured (`bench/`, JMH `-f 2 -wi 3 -i 5 -prof gc`, JDK 26, Scala 3.9; `bench.d/2026-10-17_three.csv`; the
machine before its in-place handlers in `bench.d/2026-10-17_free-vs-machine.csv`), one run of each program, n = 1000:

| Program | `free` | `machine` | `okaycore` |
|---|---|---|---|
| 1000 maps, no operation | 10.1 µs, 126 KB | 10.4 µs, 126 KB | 10.7 µs, 126 KB |
| 1000 `ask`s under their handler, each with a map pending | 2272 µs, 24.7 MB | 23.9 µs, 270 KB | 16.0 µs, 206 KB |
| the same across four handlers of other effects | 2448 µs, 26.7 MB | 26.1 µs, 271 KB | 31.0 µs, 375 KB |
| 1000 `get`/`put` pairs under `State` | 105 µs, 1.36 MB | 23.9 µs, 286 KB | 14.7 µs, 198 KB |

`free` is quadratic when maps are pending: at each operation it rebuilds `k` from every frame on its stack. In the
machine an operation answered in place costs the same however many handlers it crosses (23.9 against 26.1 µs);
okay-core's is faster at its handler and pays more for each handler crossed (16.0 against 31.0 µs, 206 against
375 KB).

## Build

```sh
sbt testAll        # both compilers, okay-core's machine, and the three compatibility checks
sbt suite/test     # Scala 3
sbt "++2.13.18 suite/test"
python3 /tmp/okz/tools/withlock.py sbt "bench/Jmh/run -f 2 -wi 3 -i 5 -prof gc"   # JMH, holding JMH's lock
```
