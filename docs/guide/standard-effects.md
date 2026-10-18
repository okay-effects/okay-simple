# The standard effects

`fx.std` — `import fx.std.*` — written once over the interface, for both compilers; their definitions in the object
`okay.effects.Std`, so other effects of the same names do not clash. Each handler takes its effect from anywhere in
the row; the effect's parameter is given, the rest inferred: `writer[String](p)`.

```scala
import okay.effects.Std
import okay.effects.*
import okay.effects.std.*
```

| Effect | Operations | Handlers |
|---|---|---|
| `State % S` | `get`, `put(s)`, `modify(f)`, `gets(f)`, `getAndSet(s)` | `state(s0)`: the final state and the value |
| `Reader % E` | `ask` | `reader(e)`; `local(f)(p)`: `p` sees `f(e)` |
| `Writer % W` | `tell(w)` | `writer[W]`: what was told, in order; `listen[W](p)`, `censor(f)(p)` |
| `Error % E` | `raise(e)` | `attempt[E]`: `Either[E, A]`; `recover[E](p)(h)` |
| `Abort` | `abort` | `optional(p)`: `Option[A]` |
| `Choose` | `among(xs)`, `fail`, `guard(cond)` | `all`: every way; `first`: the first way with no `fail` |
| `Yield % O` | `emit(o)` | `collect[O]`, `take[O](n)`, `each[O](p)(f)` |
| `Fresh` | `fresh` | `freshFrom(n0)` |
| `Random` | `nextLong`, `nextInt(bound)`, `nextDouble` | `random(seed)` |
| `Store[K, V, *]` | `load(k)`, `save(k, v)`, `remove(k)` | `store(m0)`: the final map and the value |
| `Lift[G, *]` (Scala 3: `[x] =>> Lift[G, x]`) | `lift(ga)` | a `Runner`'s `lower[G](p)`, given a `Lowering[G]` |
| `Control % (B ! R)` | `shift(f)` | `reset(p)` — see [delimited control](delimited-control.md) |
| `observe`, `around` | — | over any effect: see [interception](interception.md) |

## The words of one row: `On[R]`

The operations take the row as a type argument, `get[Int, R]`, because a program's first operation has nothing to
infer it from. A program of one row names it once instead: in an object extending `std.On[R]` (or after
`val words = std.on[R]; import words.*`) the words are the standard ones at `R` — `ask[E]`, `get`, `put(s)`,
`modify[S](f)`, `tell(w)`, `raise(e)`, `among(xs)`, `fail`, `pure(a)` — and only what the row does not decide is
left to give. A state's type is the row's when the row has one state, so `get` alone is enough; `raise(e)` is a
program of no value, `Nothing ! R`, so of any.

```scala
object RowWords:
  case class Config(greeting: String, limit: Int)
  type Service = Reader % Config + Writer % String + State % Int

  object service extends std.On[Service]:
    def serve(user: String): String ! Service =
      for
        c <- ask[Config]
        n <- get
        reply <- if n >= c.limit then tell(s"refused $user: busy").as("try later")
                 else put(n + 1).andThen(tell(s"served $user")).as(s"${c.greeting} $user")
      yield reply

  val day = for a <- service.serve("ann"); b <- service.serve("bob"); c <- service.serve("cid") yield List(a, b, c)
  val result = run(state(0)(writer[String](reader(Config("hello", 2))(day))))

assert(RowWords.result == (2, (List("served ann", "served bob", "refused cid: busy"), List("hello ann", "hello bob", "try later"))))
```

Scala 2 the same: `object service extends std.On[Service] { ... }`, the row by names, `get[Int]`.

## State

```scala
type S = State % Int
val counter: Int ! S = for _ <- modify[Int, S](_ + 1); n <- gets[Int, Int, S](_ * 10); old <- getAndSet[Int, S](0) yield n + old
assert(run(state(4)(counter)) == (0, 55))
```

Answered in place. A `k` resumed twice by a handler outside the state resumes the state of its time.

## Reader

```scala
type Rd = Reader % Int
val seen: (Int, Int, Int) ! Rd =
  for a <- ask[Int, Rd]; b <- local[Int](_ * 2)(ask[Int, Rd]); c <- ask[Int, Rd] yield (a, b, c)
assert(run(reader(21)(seen)) == (21, 42, 21))
```

`local(f)(p)` needs `Sub[Rest, R]`: `p` handled by a reader of its own, its other effects back where they were —
found by itself for a row named in full.

## Writer

```scala
type Wr = Writer % String
val told: Int ! Wr =
  tell[String, Wr]("a").andThen(listen[String](tell[String, Wr]("b").andThen(tell[String, Wr]("c")).as(1)).map(_._1.size))
assert(run(writer[String](told)) == (List("a", "b", "c"), 2))
```

`listen` and `censor` tell outside what was told inside when the inner program ends — not one by one.

## Error and Abort

```scala
type Er = Error % String
def safeDiv(a: Int, b: Int): Int ! Er = if b == 0 then raise[String, Er, Int]("div by zero") else pure(a / b)
assert(run(attempt[String](safeDiv(1, 0))) == Left("div by zero"))
assert(run(recover[String](safeDiv(1, 0))(_ => pure(0))) == 0)
assert(run(optional(abort[Int, Abort].map(_ + 1))) == None)
```

Both handlers ABORT: the rest of the program is dropped, a handler outside never sees it. A state handled outside
the error keeps what was put before the raise; one handled inside is dropped with the rest.

## Choose

```scala
def queens(n: Int): List[Int] ! Choose =
  (0 until n).foldLeft(pure[List[Int], Choose](Nil)) { (p, _) =>
    p.flatMap(qs => among[Int, Choose]((0 until n).toList).flatMap { q =>
      guard[Choose](qs.zipWithIndex.forall((c, i) => c != q && (c - q).abs != i + 1)).as(q :: qs)
    })
  }
assert(run(all(queens(6))).size == 4)
assert(run(first(queens(8))).isDefined)
```

`all` resumes `k` for each choice — linear in the ways. `first` tries them in order and stops at the first.

## Yield

```scala
type Y = Yield % Int
def nats(n: Int): Unit ! Y = emit[Int, Y](n).andThen(nats(n + 1))
assert(run(take[Int](3)(nats(0))) == (List(0, 1, 2), None))

type YS = State % Int + Yield % Int
def three: Unit ! YS = emit[Int, YS](1).andThen(emit[Int, YS](2)).andThen(emit[Int, YS](3))
assert(run(state(0)(each[Int](three)(o => modify[Int, State % Int](_ + o))))._1 == 6)
```

`take(n)` stops the generator at its `n + 1`-th value: an endless generator taken as far as asked. For streams with
transformations, see the [stream module](../modules/stream.md).

## Fresh, Random, Store

```scala
assert(run(freshFrom(100)(fresh[Fresh].flatMap(a => fresh[Fresh].map(b => (a, b))))) == (100L, 101L))

def roll: Int ! Random = nextInt[Random](6).map(_ + 1)
val rolls = (1 to 5).toList.foldLeft(pure[List[Int], Random](Nil))((p, _) => p.flatMap(l => roll.map(_ :: l)))
assert(run(random(7)(rolls)) == run(random(7)(rolls)))                // the same seed, the same numbers

type KV = [x] =>> Store[String, Int, x]
val stored = save[String, Int, KV]("a", 1).andThen(load[String, Int, KV]("a"))
assert(run(store[String, Int](Map.empty)(stored)) == (Map("a" -> 1), Some(1)))
```

`Random` is splitmix64 from the seed, its state the handler's: deterministic, and a `k` resumed twice resumes the
generator of its time.

## Lift and lower

`lift(ga)` performs a `G[X]` of another monad inside a program; a `Runner`'s `lower[G](p)` gives a program whose one
effect is `Lift[G, *]` back as a `G[A]`, each lifted `G` bound in `G`, given a `Lowering[G]` (its `pure` and bind):

```scala
import okay.effects.machine.Machine

given Std.Lowering[Option] with
  def pure[A](a: A): Option[A] = Some(a)
  def flatMap[A, B](ga: Option[A])(f: A => Option[B]): Option[B] = ga.flatMap(f)

type LO = [x] =>> Lift[Option, x]
val sum = lift[Option, Int, LO](Some(1)).flatMap(a => lift[Option, Int, LO](Some(2)).map(_ + a))
assert(Machine.lower[Option, Int](sum) == Some(3))
```

The interop modules give `Lowering`s of cats' monads and of ZIO's `Task`: see [interop](../modules/interop.md).

## Better handlers of an implementation

`Std` is a class over the implementation, made by its `newStd`: an implementation may offer its own `Std` with
handlers of its own. Every handler above that answers in place is the implementation's `handle` of that shape, so
what an implementation does better it does for the standard effects too.
