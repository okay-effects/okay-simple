# Imports and defaults

One import line per part, and nothing else to set up: the machine is the default implementation.

| Import | gives |
|---|---|
| `okay.effects.*` | a program's types and words on the machine: `!`, `Pure`, `In`, `Split`, `Sub`, `Has`, `+` (and the package's own `%`, `*`, `Sum`, `Void`, `Cell`), `pure`, `map`, `flatMap`, `delay`, `tailcall`, `tailRecM`, `perform`, `handle` of each shape, `handleLast`, `split`, `embed`, `intercept`, `run`, `lower`; the handler shapes; a program's methods |
| `okay.effects.std.*` | the standard effects and their handlers, on the machine |
| `okay.effects.delimited.*` | delimited control changing answer types, on the machine — in a scope of its own: its `pure`, `map`, `flatMap` are `Cps`'s |
| `okay.effects.stream.*`, `.async.*`, `.concurrent.*`, `.resource.*`, `.world.*`, `.probability.*`, `.parsing.*`, `.logic.*`, `.transaction.*`, `.resilience.*` | each module on the machine |
| `okay.effects.catsinterop.*`, `.ziointerop.*`, `.zstream.*`, `.fs2interop.*`, `.monixinterop.*`, `.jstream.*`, `.tofuinterop.*` | each interop on the machine |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.stream.*
import okay.effects.concurrent.*

type T = Writer % String + Coop % Int
def worker(name: String, n: Int): Unit ! T =
  if n == 0 then pure(()) else tell[String, T](s"$name$n").andThen(pass[Int, T]).andThen(worker(name, n - 1))

assert(run(writer[String](scheduled[Int](List(worker("a", 2), worker("b", 1)))))._1 == List("a2", "b1", "a1"))
assert(run(range[Pure](0, 3).toList) == List(0, 1, 2))
```

The modules on the machine work together: the stream modules — `stream`, `async`'s `mergeAsync`, `fs2interop`,
`zstream`, `jstream`, `monixinterop` — are over one default streams instance, so a stream of one is a stream of all.

## How it is made

- `okay.effects.*`: the machine module gives the package its words — in Scala 3 by top-level `export`s of the
  machine's; in Scala 2 by a package object of forwarders. The interface's module knows no implementation.
- A module's package: the module's class made on the machine — in Scala 3 a top-level `val onMachine`, its members
  exported; in Scala 2 a package object extending the class.
- The words are the machine's own, called straight: nothing between them and the machine.

## Another implementation

Each module is also a class over the implementation in the implicit context, made by its companion, which keeps the
implementation's type so the programs it makes run on it:

```scala
import okay.effects.Runner
import okay.effects.stream.Streams

object OnFree:
  given Runner = okay.effects.free.Free
  val s = Streams()
  import s.*, s.fx.*

  val evens: List[Int] = run(range[Pure](0, 10).filter(_ % 2 == 0).toList)

assert(OnFree.evens == List(0, 2, 4, 6, 8))
```

The interface itself on another implementation: `import okay.effects.free.Free.*`, `import okay.effects.free.Free.std.*`
— or code written over `Effects`, as in [getting started](../getting-started.md#the-same-program-on-another-implementation).

## Names

- So that nothing shadows a root package after `import okay.effects.*`, the interop packages are `catsinterop`,
  `ziointerop`, `tofuinterop`, `fs2interop`, `monixinterop` (not `cats`, `zio`, `tofu`, `fs2`, `monix`), and the
  console, the clock and the environment are `world` (not `io`: `io.circe` and the like stay yours).
- A module's class holds its implementation in a value, `fx`, not an implicit: a package of defaults gives no
  implementation to the code it is imported into.
- Two modules' packages may give a name both (`fx`, `onMachine`): import what you use, or name it by its package.
- In Scala 2, inside one of these packages, its own members win over imports — code of yours is in packages of yours.
