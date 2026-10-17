# resource — scopes and finalizers

`okay-effects-resource`, `okay.effects.resource`. A scope runs its finalizers when it ends, the last deferred first.

| | |
|---|---|
| effect | `Scope[P, X]`, `P` the finalizers' program type: `Scope % (Unit ! Rest)` |
| operations | `defer(fin)`: `fin`, a program of the row outside the scope, run at its end |
| combinator | `bracket(acquire)(release)(use)`: acquired, its release deferred, used |
| handler | `scoped(p)`: `p`, then its finalizers |

```scala
import okay.effects.machine.Machine
import okay.effects.resource.{Resources, Scope}

val r = Resources(Machine)
import r.*, r.fx.*, r.fx.std.*

type Out = Writer % String
type In0 = Writer % String + Scope % (Unit ! Out) + Error % String

def open(name: String): String ! In0 = tell[String, In0](s"open $name").as(name)
def close(name: String): Unit ! Out = tell[String, Out](s"close $name")

def work(fail: Boolean): Int ! In0 =
  bracket[String, Int, Out, In0](open("a"))(close)(a =>
    bracket[String, Int, Out, In0](open("b"))(close)(b =>
      tell[String, In0](s"use $a $b").andThen(if fail then raise[String, In0, Int]("boom") else pure(2))))

assert(run(writer[String](scoped(attempt[String](work(false))))) ==
  (List("open a", "open b", "use a b", "close b", "close a"), Right(2)))
assert(run(writer[String](scoped(attempt[String](work(true))))) ==
  (List("open a", "open b", "use a b", "close b", "close a"), Left("boom")))
```

## Semantics

- Finalizers run when the scope's program ends with its value. An error HANDLED INSIDE the scope —
  `scoped(attempt(p))` — ends it with a `Left`, so the finalizers run.
- An error handled OUTSIDE the scope drops the rest of the scope, its finalizers with it: handle errors inside.
- A `k` resumed twice inside a scope ends it twice, each end running the finalizers deferred before it: scopes are
  for programs that resume once.
