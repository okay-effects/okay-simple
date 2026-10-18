# Writing an implementation

An implementation is a `Runner`: the interface's types and its primitives. Everything else — the five shapes of
handler at any place, `intercept`, the standard effects, delimited control — is written once on them, and an
implementation may override any of it to do better.

## What to give

| Member | |
|---|---|
| `type ![+A, R[_]]` | a program |
| `type In[E[_], R[_]]`, `type Split[E[_], R[_], Rest[_]] <: In[E, R]`, `type Sub[R[_], S[_]]` | the evidence, whatever you like |
| `splitOne`, `splitLast`, `splitInit`, `subPure`, `subOne`, `subMore` | the evidence's derivations, implicit |
| `pure`, `flatMap`, `delay` (`map` optional) | the monad, in constant stack |
| `perform[E, R, X](op)(in)` | an operation at its place (protected: `perform[E](op)` comes here) |
| `split`, `embed` | `E` moved to the end; a program in a bigger row |
| `interposed` | `E` moved to a new last place, the rest where it is — for `intercept` |
| `handleLast(p, h: Handler)` | the general handler of the last place, deep, `k` resumable any number of times |
| `run` | a program of no effect run |

## What to override, to be fast

- `handleLast` and `handle` of the other four shapes: a handler answering in place needs no `k`; one aborting needs
  no rest; a stateful one keeps its state in place of the defaults' functions of the state.
- `handle` of each shape at its place, without `split`: the machine and `Free` take an effect from its place at once.
- `newStd`, a `Std` of your own; `newDelimited`, delimited control of your own (okay-core's bridge has it native).

## Places for Scala 3

`okay.effects.Places` gives a typed representation of the evidence, for a Scala 3 implementation with no cast: a
place a path (`Last`, `Init`, `Only`, `Absurd`), `Split` the place itself, `Sub` a `Renaming`; `take`, `toEnd`,
`interposing` the renamings a handler needs. Matching a place against a handler's level tells, by GADT inference,
that an operation is the handler's own.

## Check it

The interface's suite is a library, `okay-effects-tck`: one class over any `Runner`, every behaviour of the
interface — handlers of each shape, multi-shot `k`, state through branches and in each, splits, embeddings, errors,
generators, delimited control, interception, constant stack.

```scala-sbt
libraryDependencies += "io.github.okay-effects" %% "okay-effects-tck" % "0.1.0" % Test
```

```scala
class MyImplementationSuite extends okay.effects.InterfaceSuite(okay.effects.machine.Machine)
```

okay-core's and okay-kernel's bridges are checked so, and tagless (`Tagless.on(Machine)`).
