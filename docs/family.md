# The okay family

okay-simple is one of a family of projects about algebraic effects and handlers. Each project is its own repository,
all of them in the GitHub organization [okay-effects](https://github.com/okay-effects), and they meet at two places:

- **okay-core**: the machine that the others port, test against and bridge to;
- **okay-kernel**: a protocol that lets implementations in different languages run each other's programs under their
  own handlers, in one process or over a wire.

| Project | Language | What it is | Meets okay-simple |
|---|---|---|---|
| [okay-core](https://github.com/okay-effects/okay-core) | Scala 3 (and 2.13) | the kernel: one monad `Cont[R, I, O, A]`, delimited control, a CEK machine with `k` as data | module `effects/`: `okay.effects.core.Core`, a `Runner` |
| [okay-kernel](https://github.com/okay-effects/okay-kernel) | Java API, Scala, JS | the protocol: a program steps to `Done`, `Yield(op, k)`, `Failed`, `Cancelled`; a TCK | module `effects/`: `okay.effects.kernel.OnKernel.on(effects)` |
| [okay](https://github.com/okay-effects/okay) | Scala 3 | the platform: effects and over a hundred libraries on them — HTTP, SQL, streams, agents, cluster, durable workflows | through okay-core, its kernel |
| [okay-java](https://github.com/okay-effects/okay-java) | Java 25 | okay-core's design in Java, on `shift0`; an implementation of okay-kernel's protocol | through okay-kernel |
| [okay-go](https://github.com/okay-effects/okay-go) | Go | direct-style effects on Go's coroutines, capabilities; the protocol, a wire to okay | through okay-kernel |
| [okay-rust](https://github.com/okay-effects/okay-rust) | Rust | okay-core's kernel in Rust (crate `okay`), and effects in async Rust (crate `okay-effects`) | through okay-kernel |
| [okay-python](https://github.com/okay-effects/okay-python) | Python 3.11+ | typed effects on generators, `mypy --strict` clean; the protocol, a wire to okay | through okay-kernel |
| [okay-ts](https://github.com/okay-effects/okay-ts) | TypeScript | okay-core's effects in TypeScript, programs as data or generators; the protocol | through okay-kernel |
| [okay-lean](https://github.com/okay-effects/okay-lean) | Lean 4 | this repository's `Free` in Lean: programs as data, rows of typed places, the laws proved; a far side of okay's wire, over pipes and in the JVM's own process through FFM | a port of its `Free`; okay's wire |
| [okay-zig](https://github.com/okay-effects/okay-zig) | Zig 0.17 | okay-scala's design in Zig: rows as tuples of types, a CEK machine whose `k` resumes any number of times, and contexts — a program a plain function given its handlers — with observers and guards as error unions; okay-kernel's TCK digest; a far side of okay's wire, over pipes and through FFM | okay's wire |
| [okay-scala](https://github.com/okay-effects/okay-scala) | Scala 3 | the rows `A ! R`, `R + E`, `F % S` for Scala 3 alone: one CEK machine with generalised continuations, enums and GADTs, no cast; JVM, Scala.js, Native | module `cek/`: `okay.effects.cek.OnCek`, a `Runner`; no class shared |
| [okay-proof](https://github.com/okay-effects/okay-proof) | Scala 3 | a small LCF-style prover for the run time: exact arithmetic, linear arithmetic and the Omega test by certificate, inference steps as an effect for audit and control | built on it (`okay-effects-machine` 0.1.0): a proof a program of its effects |

okay-simple gives the family ONE typed Scala interface, `Effectful`, written over any machine. Its own
implementations are the machine, `Free` and tagless. okay-core's machine and okay-kernel's protocol are two more,
each a bridge in its own repository, checked by this repository's suite (`okay-effects-tck`).

## okay-core — the kernel

A Scala 3 library of algebraic effects and handlers with delimited control. It runs on the JVM, Scala.js and Scala
Native, and can be used from Scala 2.13. Everything is one monad, `Cont[R, I, O, A]` in package `okay.one`:

- `R` is a row of effects;
- control is Materzok and Biernacki's `shift0` and `$`;
- the machine is a CEK machine `eval(c, k, m)` whose continuation `k` is data, so `k` resumes any number of times.

There are no macros and no casts: an operation carries a typed path to its effect in the row. The facade over the
monad is `A ! E1 + E2`. The handler forms are `Answering`, `Stateful`, `Translating`, `Aborting` and `Deep`.
Effects marked sensitive can be handled only under `audit`, and an observer sees every operation.

```scala excerpt
import okay.*
def greet: Int ! Reader % String + Writer % String =
  ask[String].flatMap(name => tell(s"hello, $name").map(_ => name.length))
def twice[T <: Row]: Eff[T, Int] =
  reset[Int, T](shift[Int, T, Int](k => k(10).flatMap(k)).at[Shifts[Int, T] +: T].map(_ + 1))
run(reader("Ann")(writer[String, Int, Reader % String +: Pure](greet)))   // (List("hello, Ann"), 3)
run(twice[Pure])                                                         // 12
```

**Modules:**

- `core`: the kernel and the facade;
- `kernel`: `OnCore`, okay-kernel's protocol on the one monad;
- `effects`: `okay-effects-core`, this interface on okay-core's machine;
- `async`, `tx`;
- interop with cats and tofu;
- Scala 2.13 builds.

**With okay-simple:** `okay.effects.core.Core` is a `Runner`. A program is okay-core's own `Cont`, and places are
okay-core's paths. A handler becomes `Deep`, `Answering` or `Stateful`; `Delimited` is okay-core's native `shift0`.
Across handlers it is the fastest of the implementations on operations: `State` takes 14.3 µs per 1000 against the
machine's 18.5.

## okay-kernel — the protocol

The protocol lets effect implementations work together: a program written for one runs under handlers written for
another, in another language. The API is pure Java with no dependencies.

An implementation hands out a program as a `Computation`, which steps to:

- `Done`;
- `Yield(op, k)`, where `k` is multi-shot: `resume`, `fail`, `cancel`;
- `Failed`;
- `Cancelled`.

An effect's identity is an interned `Type` / `Key<F>`, and operations find their handlers by it at run time.

```java
interface Op<X>           { Type effect(); }
interface Computation<A>  { Step<A> start(Answers host); }
sealed interface Step<A>  permits Done, Yield, Failed, Cancelled {}
record Yield<X, A>(Op<X> op, Resume<X, A> k) implements Step<A> {}
interface Resume<X, A> { Step<A> resume(X x); Step<A> fail(Throwable e); Step<A> cancel(); }
```

The handler shapes are `Answering`, `Aborting`, `Returning`, `Translating` and `General`.

**Modules:**

- `api`: the protocol;
- `steps`: the reference machine (JVM, JS, Native);
- `tck`: the laws, the protocol, and 1000 random programs checked by one digest;
- `js`: the protocol as an ES module;
- `integration`: the TCK on every implementation in one process;
- `effects`: `okay-effects-kernel`.

Its README measures the protocol's mixed program, 1000 orders, in µs:

| Implementation | Protocol's program | Its own program |
|---|---|---|
| okay-java | 237 | 102 (cell) / 172 (state) |
| okay-core's `OnCore` | 286 | 223 |
| steps (reference) | 535 | — |

**With okay-simple:** `OnKernel.on(effects)` is a `Runner` over any implementation of the protocol: places are
indexes, and a handler becomes the protocol's `general` or `answering`. So a program of okay-simple runs on okay-java,
okay-core, or the reference machine.

## okay — the platform

"Okay! Extensible effects for Scala." A program's type says what it needs:
`type Source[W] = Unit ! Writer % W + Async`. The core has no dependencies, is one source for the JVM (JDK 21+, Loom),
Scala.js and Scala Native, and is direct style with no macros. `A ! R` is okay-core's monad, and `Effects[M]` is
okay-core's `RowEffects`.

```scala excerpt
val prog: Int ! State % Int =
  for
    x <- State.get[Int]
    _ <- State.set(x + 40)
    y <- State.get[Int]
  yield y + 2
val ran = prog.handle(State(0)).run   // (40, 42)
```

Most of the repository is libraries on the effects, over a hundred modules:

- HTTP: Jetty, Netty;
- SQL and JDBC;
- streams and chunks;
- STM, a cluster, durable workflows;
- LLMs, RAG, agents, MCP;
- a UI;
- interop with cats, ZIO, kyo, fs2, Spark, Flink, Kafka.

okay is also a host for the other languages: its foreign workers and wire run programs of okay-go, okay-python,
okay-rust and okay-ts under Scala handlers. Its README measures:

- 10k `flatMap`s: 5.5 µs (kyo 60, cats IO 163, ZIO 193);
- a stream pipeline: 8.2 µs (fs2 21.9, ZIO 35.8).

**With okay-simple:** through okay-core, okay's kernel. okay-simple's `Effectful` is the same idea, a typed interface
over any machine, kept small and for both Scala compilers.

## okay-java

Delimited continuations and effects on `shift0` in Java 25: okay-core's design in Java, and an implementation of
okay-kernel's protocol. A program is `Eff<L, A>`, where the level `L` stands for the handlers in force. A handler's
body runs at a fresh level (rank-2), so a capability that escapes its handler is a compile error. The machine runs in
constant stack over an explicit stack of delimiters. `k` is data and multi-shot.

```java
Log<String, Integer> log = Eff.run(Writer.writer(new Writer.Body<Top, String, Integer>() {
  public <M extends Top> Eff<M, Integer> in(Writer<M, String> w) {
    return w.tell("a").then(w.tell("b")).map(_ -> 2);
  }
}));
```

The standard effects are `Reader`, `State`, `Writer`, `Throws`, `Choose`, `Async` (`fork`, `blocking`) and `Emit`
(streams, iterators). There is an optional Spring Boot module. `Eff` is okay-kernel's `Prog`, with no wrapper, and
okay-kernel's TCK runs on it.

## okay-go

Algebraic effects for Go in direct style, with capabilities, on Go's own coroutines (`iter.Pull`). Performing an
operation suspends the program, and the handler owning the capability answers it: 49 ns per operation, no allocation.

- Handlers answer by identity, not by type, so two instances of one effect are told apart, and an escaped capability is
  refused.
- Continuations are one-shot (a coroutine cannot be copied); package `prog` has programs as data, multi-shot.
- Parallel branches run on goroutines, their operations sent over channels to the handler's goroutine.

```go
got := okay.Main(func(r *okay.Run) int {
    return okay.Answer(r, func(Ask) int { return 1 }, func(outer okay.Cap[Ask]) int {
        return okay.Answer(r, func(Ask) int { return 2 }, func(inner okay.Cap[Ask]) int {
            return okay.Perform(outer, Ask{})*10 + okay.Perform(inner, Ask{})
        })
    })
})
fmt.Println(got) // 12
```

It implements okay-kernel's protocol, and has middleware (`Drive`, `Routes`, `Record`, `Replay`) and a `wire` to
okay: a shop runs in Go while its inventory, payments and mail are Scala handlers in okay.

## okay-rust

okay's algebraic effects in Rust, with no dependencies, in two crates. (It was **okay-rs**; okay-rs is retired, its
history kept here.)

**Crate `okay`: okay-core's kernel in Rust.**

- A program is data, `Eff<R, A>`, over a type-level row `row![…]`; the compiler finds each effect's place (`Member`).
- Continuations are multi-shot; the machine keeps recursion off the native stack.
- Handlers are `answering`, `stateful`, `deep`, `aborting`, `translating`.
- Interceptors and audit; okay-kernel's step protocol; okay's wire; delimited control (`okay::cont`).
- A typed async form of `eff!`, and the macros `effect!` and `needs!`.

**Crate `okay-effects`: effects in plain async Rust with capabilities.** A handler makes a `Cap<O>`, and the body
calls `cap.perform(op).await`: about 43 M operations per second.

```rust
type R = row![Reader<i64>, State<i64>];
let p: Eff<R, i64> = eff! { data;
    n <- ask::<i64, _, _>();
    _ <- modify::<i64, _, _>(move |s| s + n);
    s <- get::<i64, _, _>();
    yield s * 2
};
assert_eq!(p.handle(reader(5)).handle(state(1)).run(), (6, 12));
```

It is checked by okay-core's suites, by a reference interpreter on random programs, and by okay-kernel's TCK digest.
Over okay's wire, a Rust program runs under Scala handlers, and okay's `Choice` resumes a Rust rest for every branch.

## okay-python

`okay-effects` for Python 3.11+, with no dependencies: typed algebraic effects on generators, `mypy --strict` clean.
A program is a generator, `Eff[A]`, and performs through a capability: `yield from cap.perform(op)`, typed by what the
operation answers.

- Handlers answer by identity, as in okay-go.
- Continuations are one-shot (a generator cannot be copied); `every_path` replays the program to resume the rest with
  each answer.
- Async runs on asyncio, with no coloured functions.

```python
def reader(v: T, body: Callable[[Cap[Ask[T]]], Eff[A]]) -> Eff[A]:
    return (yield from answering(Ask, lambda _: v, body))

def prog(outer: Cap[Ask[int]]) -> Eff[int]:
    def inner_body(inner: Cap[Ask[int]]) -> Eff[int]:
        a = yield from outer.perform(Ask[int]())
        b = yield from inner.perform(Ask[int]())
        return a * 10 + b
    return (yield from reader(2, inner_body))

run(reader(1, prog))  # 12
```

It implements okay-kernel's protocol, has middleware, and a wire to okay: a shop runs in Python under Scala handlers.

## okay-ts

okay-core's effects in TypeScript, with no runtime dependencies. A program is `Eff<A, E, M>`, and its row is a union
type `E`: `run()` exists only when the row is empty. Effects are tags, by name at run time and by type at compile
time (`Reader<"Config", number>`). Continuations come two ways, shown in `M`:

- generators (`eff(function* …)`) are one-shot;
- programs as data (`pure`, `flatMap`, `Do`) are multi-shot — `Choice.all()` takes only these.

```ts
const Config = Reader("Config", of<number>())
const Counter = State("Counter", of<number>())
const program = eff(function* () {
  const n = yield* Config.ask()
  yield* Counter.modify((s) => s + n)
  const s = yield* Counter.get()
  return s * 2
})
program.handle(Config.value(5)).handle(Counter.from(1)).run() // [6, 12]
```

Interceptors and audit follow okay-core's (`audit`, `intercept`, `observe`). The tests are okay-core's suites. It
speaks okay-kernel's protocol (`fromKernel`/`toKernel`, with okay-kernel's Scala.js module), and is a far side of
okay's wire, over stdio or a WebSocket.

## okay-lean

This repository's `Free` monad in Lean 4: a program is data (`ret`, `bind`, an operation alone, an operation and its
rest, `delay`), its row a list of effects, an operation carrying its typed `Place` in it — a handler takes its effect
from its place by dependent matching, no cast, as here by Scala 3's GADT inference. Each interpreter is written twice:
a total reference in continuation-passing style, which the proofs are about, and this repository's loop for running it
(`@[implemented_by]`: a bind re-associated as data, every step a jump, a deep handler's `k` a trampoline) — so the
monad laws are theorems (`rfl` after normalization), and 10^8 operations run in constant stack.

```lean
import Okay
open Okay.Effects Okay.Effects.Free

def greet [In (Reader String) R] [In (Writer String) R] [In (State Nat) R] : Nat ! R := do
  let name : String ← ask
  tell s!"hello, {name}"
  let n : Nat ← get
  put (n + name.length)
  pure n

#eval run (writer String (state 10 (reader "Ann" (greet (R := [Reader String, State Nat, Writer String])))))
-- ((10, 13), ["hello, Ann"])
```

The standard effects — `State`, `Reader`, `Writer`, `Error`, `Choose`, `Abort` — with their handlers' laws proved. And
the wire: a Lean worker is a far side of okay's (`Okay.Wire`: programs by name, typed effects through `Wire`, a rest a
pure function continued any number of times), over a child's pipes or as one library of okay's C ABI that okay loads
into the JVM's own process through FFM, checked by okay's conformance suite over both; and Lean as the host of another
side's program, its operations under Lean's handlers.

## okay-scala

This repository's interface written again for Scala 3 alone, nothing kept for Scala 2, and one machine for it: the
CEK machine with the generalised continuations of Hillerström, Lindley and Atkey, its data enums typed by the row, so
Scala 3's GADT inference proves each operation its handler's — no cast. A clause is a polymorphic function, a handler
one of four shapes; a missing effect says so as the row is written, by its one macro. It depends on nothing, and is
built on Scala 3.3 LTS for the JVM, Scala.js and Scala Native. Its machine takes places of any representation, so this
repository's adapter, `cek`, runs the interface on it with this interface's own rows and places: no class shared.

```text
def greet[R[_]: Has[Reader % String] : Has[State % Int] : Has[Writer % String]]: Int ! R =
  for
    name <- Reader.ask
    n    <- State.get
    _    <- State.put(n + 1)
    _    <- Writer.tell(s"hello, $name")
  yield n
```

## okay-proof

A small prover for decisions that must be justified, run inside an application: a kernel of first-order logic with
equality, induction and exact arithmetic, LCF-style — a theorem is a value only the kernel makes. Its inference step
is an effect of this repository's, `Infer.Step(rule)`, the kernel its handler: `observe` audits every step, `around`
vetoes a rule, and no interceptor can forge a theorem (an answer must be made from the very step, by the kernel that
owns the theory). Linear arithmetic over the rationals and the integers (the Omega test) and products of variables are
decided outside the kernel and checked inside, by certificate; proof search runs on `Choose`, its strategy a handler.
A decision proved in microseconds; its certificate, JSON with the theory and its digest, checked again by okay-lean's
second kernel in Lean.

```text
Auto.prove(bank, facts, P(mayTransfer, List(ann, acc1, dec("700.00"))))   // Right(Some(t)): t.hyps the facts it rests on
Certificate.checkAlone(Certificate.of(t))                                 // every step derived again
```

## Continuations across the family

| | multi-shot `k` | how |
|---|---|---|
| okay-simple, okay-scala, okay-core, okay-kernel, okay-java, okay-rust (`okay`) | yes | `k` is data |
| okay-ts | data: yes; generators: no | two kinds of program, told apart by type |
| okay-lean | yes, across a process and through FFM too | `k` a pure function, kept by the far side under an id |
| okay-go | coroutines: no; `prog`: yes | a coroutine cannot be copied |
| okay-python, okay-rust (`okay-effects`) | by replay | the program run again, the answers so far replayed |
