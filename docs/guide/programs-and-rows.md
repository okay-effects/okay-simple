# Programs and rows

## A program

`A ! R` is a program of value `A` over the row `R`: run, it gives an `A`, and on its way it performs operations of
the effects of `R`. It is a value — a description: building it runs nothing; `run` runs it, once every effect is
handled.

The words of every program, on any implementation `fx` (`import fx.*`):

| Word | |
|---|---|
| `pure(a)` | the program of value `a`, no operation |
| `p.flatMap(f)`, `p.map(f)`, `for ... yield` | sequencing |
| `p.as(b)`, `p.andThen(q)` | the value replaced; `q` after `p` |
| `delay(p)` | `p` built when the run gets to it |
| `perform[E](op)` | the operation `op` of the effect `E` |
| `p.into[S]` | `p` in a row `S` of more effects |
| `run(p)` | a program of no effect run (a `Runner`'s) |

Every implementation runs programs in constant stack: binds nested either way, 100 000 operations under a handler.

## An effect

An effect is a type constructor, the GADT of its operations: `E[X]` an operation answered by an `X`. In Scala 3 an
`enum`, each case its answer:

```scala
import okay.effects.machine.Machine.*
import okay.effects.machine.Machine.std.*

enum Console[X]:
  case Print(line: String) extends Console[Unit]
  case Read extends Console[String]
```

Code written for both compilers gives each operation its answer as a function — `answer: S => X` — because Scala 2
does not infer a GADT's type in a match on a method's type parameter (see [Scala 2 and 3](scala-2-and-3.md)). The
standard effects are written so: `State.Get(answer: S => X)`.

## A row

A row is `Pure` (no effect), one effect, or `R + E` — the effects summed left to right:

```scala
type One = Console
type Two = Console + State % Int
type Three = Console + State % Int + Reader % String
```

`%` gives an effect its parameter: `State % Int` is `[X] =>> State[Int, X]`. `*[F]` is `F` itself, kept for code
written alike for both compilers. Underneath, `R + E` is `[X] =>> Sum[R, E, X]` and `Pure` is `Void` — phantoms,
of no operation of their own.

A row lists the effects a program may still perform. An effect may be in a row twice: two `State % Int` are two
states, each its own place.

## Evidence

A program says which effects it needs by evidence, found by implicit search:

| Evidence | means | used by |
|---|---|---|
| `In[E, R]` | `E` is in `R` | `perform`, every operation |
| `Split[E, R, Rest]` | `E` is in `R`, and `R` without it is `Rest` (a `Split` is an `In`) | every handler |
| `Sub[R, S]` | every effect of `R` is in `S` | `into`, `embed` |

`Has[E]` is the context bound for `In`: `def f[R[_]: Has[Reader % Int]]`. The evidence is abstract — each
implementation makes it what it likes (a typed path, a number, a key) — so code written over it runs on any.

For a row named in full the evidence is found where it is used; code generic in its row takes it as a parameter and
passes it on:

```scala
def total[R[_]: {Has[Reader % Int], Has[State % Int]}](n: Int): Unit ! R =
  if n == 0 then pure(()) else ask[Int, R].flatMap(x => modify[Int, R](_ + x)).andThen(total[R](n - 1))

assert(run(state(0)(reader(3)(total[Reader % Int + State % Int](10))))._1 == 30)
```

Passing it on is also the fast way: the evidence is made once, not at each operation.

## Moving between rows

- `p.into[S]` (`embed`): a program of `R` in a bigger row `S`, given `Sub[R, S]`.
- `split(p)`: `E` taken from its place to the end, `Rest + E` — what a handler of the last place handles
  (`handleLast`). Handlers do it themselves; you rarely need it.

```scala
val p: Int ! (State % Int) = get[Int, State % Int]
val q: Int ! (Console + State % Int) = p.into[Console + State % Int]
```

## Making programs and running them

`Effects` is the interface's words that MAKE programs; `Runner extends Effects` adds `run` (and `lower`, to another
monad). Every implementation is a `Runner`; code that only makes programs takes an `Effects`, so it runs on any
implementation and on `Tagless` (an `Effects` of no implementation). See [implementations](implementations.md).
