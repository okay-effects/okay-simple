# Implementations

One interface; several implementations, each a `Runner` (an `Effects` that runs). Code written over `Effects`
runs on any; choose where you run.

| Implementation | Module | Value | What it is |
|---|---|---|---|
| the machine | `okay-effects-machine` | `okay.effects.machine.Machine` | an explicit stack of frames and handlers' levels, an operation finding its handler in one walk — the fast one |
| the `Free` monad | `okay-effects-free` | `okay.effects.free.Free` (through the interface), `OnFree` (as it is, its words `inline`) | a program a tree, each handler its interpreter |
| tagless | `okay-effects-tagless` | `okay.effects.tagless.Tagless` (an `Effects`), `Tagless.on(fx)` (a `Runner`) | programs of NO implementation, made on the one given at `run` |
| okay-core | okay-core's `okay-effects-core` | `okay.effects.core.Core` | the interface on okay-core's machine: a program okay-core's own `Cont` |
| okay-kernel | okay-kernel's `okay-effects-kernel` | `okay.effects.kernel.OnKernel.on(effects)` | on any implementation of okay-kernel's protocol: its reference machine, okay-java, okay-core |

## The machine

A program is data, `Prog[R, A]`; the run a `@tailrec` loop over it, its frames (the binds pending) and its levels
(the handlers installed). An answer in place goes straight into the next frame; a `k` taken is data, the levels
crossed put back where it is resumed. In Scala 3 its places are typed paths and its loop has no cast; in Scala 2 a
place is a number, the walk to a handler over the levels' fields.

## The `Free` monad

`Free[F, A]` — `Return`, `Perform`, `Bind`, `Inject` (an operation and its rest), `Delay` — interpreted by each
handler in turn, `k` a trampoline. Simple and clear; slower where an operation crosses many handlers, each passing
it on. `OnFree` is the same implementation seen as it is, every word `inline`.

## Tagless

`Tagless` is an `Effects` of no implementation: a program is a `Program[A, R]`, a function of the implementation
(`on(fx)`), and so is the evidence. A program made once runs on any implementation; the implementation is asked
for at `run`, from the implicit context:

```scala
import okay.effects.Runner
import okay.effects.tagless.Tagless.*
import okay.effects.tagless.Tagless.std.*

val p: (Int, Int) ! Pure = state(1)(modify[Int, State % Int](_ * 42).andThen(get[Int, State % Int]))

assert(run(p)(using okay.effects.machine.Machine) == (42, 42))
locally { given Runner = okay.effects.free.Free; assert(run(p) == (42, 42)) }
locally { import okay.effects.machine.Machine.self; assert(run(p) == (42, 42)) }   // the implicit alone: this `run` is tagless's
```

An implementation is a VALUE, and it carries itself as an implicit of its own singleton type, `self: this.type`: so
`import okay.effects.machine.Machine.*` brings the words and, with them, the machine as the implicit implementation —
whatever asks for a `Machine.type`, a `Runner` or an `Effects` gets it: `run(p)`, `new Faces()`, `new Https()`, with
nothing more said, and the type inferred is exactly `Machine.type`. The implicit alone, when the words are another's
(tagless's `run`), is `import okay.effects.machine.Machine.self`. Mind Scala 2 when you hold an implementation of your own as an implicit
`Runner` in the same scope as an imported one: Scala 2 takes the more specific type, the imported one's; Scala 3
the nearer. Import the words of the implementation you mean, or none.

Each word, made on `fx`, is `fx`'s own: `pure` its `pure`, a handler its `handle` of that shape. One trusted step:
the `k` an implementation gives a general clause comes back as a `Program`, checked by identity — a `k` kept and run
on another implementation fails. `Tagless.on(fx)` is tagless as a `Runner`, for code that runs programs over the
interface (the interface's suite runs on it).

## The bridges

okay-core's bridge is okay-core's machine itself: a program is okay-core's `Cont`, a place its path, a handler one of
its forms (`Deep`, `Answering`, `Stateful`), delimited control its native `shift0` and `$`, interception its
observer. okay-kernel's bridge runs on any implementation of the protocol: each operation of one key carrying its
place, so two effects of one type in a row stay two places. Both live in their own repositories
(`io.github.okay-effects::okay-effects-core`, `::okay-effects-kernel`), built on the release of `okay-effects` and
`okay-effects-tck`, 0.1.0.

## Choosing

| | |
|---|---|
| speed | okay-core's bridge, then the machine; `Free` where operations do not cross many handlers |
| a program as a value of no implementation | tagless |
| clarity, a reference to compare with | `Free` |
| okay's platform, okay-kernel's runtimes | the bridges |

Measured (JMH, Scala 3, µs per program of 1000 steps; `bench/`, `bench.d/`; every column, both compilers, in the README's [Measured](../../README.md#measured)):

| Program | machine | `Free` | okay-core | tagless on the machine |
|---|---|---|---|---|
| 1000 maps | 6.9 | 12.0 | 9.1 | 10.1 |
| 1000 `ask`s under their handler | 17.0 | 33.8 | 15.9 | 24.6 |
| across four handlers of other effects | 23.5 | 70.1 | 19.1 | 39.1 |
| 1000 `get`/`put` pairs | 18.3 | 20.4 | 14.0 | 30.1 |

To write one of your own, see [writing an implementation](writing-an-implementation.md).
