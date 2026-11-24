# A tour

A walk through the library, each step a program that runs (every block here is compiled and its `assert`s run, by
`sbt docsCheck/test`). It starts where [getting started](getting-started.md) ends. All of it on the machine; the
same code runs on any implementation.

```scala
import okay.effects.machine.Machine
import okay.effects.*
import okay.effects.std.*
```

## 1. Programs and rows

A program `A ! R` gives an `A`, performing operations of the effects of the row `R`. A row is `Pure` (no effect), an
effect, or `R + E`. An effect with a parameter is written with `%`: `State % Int` is a state of `Int`.

```scala
type Counting = Reader % Int + State % Int

def step: Int ! Counting =
  for
    inc <- ask[Int, Counting]
    n   <- get[Int, Counting]
    _   <- put[Int, Counting](n + inc)
  yield n + inc

assert(run(State.run(10)(Reader.run(5)(step))) == (15, 15))
```

`ask`, `get` and `put` are operations; `Reader.run(5)` and `State.run(10)` their handlers. A program is a value:
nothing ran until `run`.

## 2. Handled in any order

A handler takes its effect from wherever it is in the row. Here the state is handled first, inside, though it is the
last of the row; the reader outside:

```scala
assert(run(Reader.run(5)(State.run(10)(step))) == (15, 15))
```

The order matters only where effects meet: a state handled inside a choice is each branch's own (section 6).

## 3. An effect of your own

An effect is a GADT of its operations — in Scala 3 an `enum`, each case typed by its answer. `perform` performs one:

```scala
enum Counter[X]:
  case Tick extends Counter[Unit]
  case Peek extends Counter[Int]

def tick[R[_]: Has[Counter]]: Unit ! R = perform[Counter](Counter.Tick)
def peek[R[_]: Has[Counter]]: Int ! R = perform[Counter](Counter.Peek)
```

Its handler is a value of one of five shapes (see [handlers](guide/handlers.md)). The simplest answers each operation
IN PLACE, with a state of its own kept in a `Cell` — a `StatefulTailHandler`:

```scala
def counting[R[_], Rest[_], A](p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
  handle(p, 0, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)]:
    def ret(n: Int, a: A) = pure((n, a))
    def answer[X](cell: Cell[Int], op: Counter[X]): X = op match
      case Counter.Tick => cell.set(cell.get + 1)
      case Counter.Peek => cell.get)

assert(run(counting(tick[Counter].andThen(tick[Counter]).andThen(peek[Counter]))) == (2, 2))
```

`Split[Counter, R, Rest]` is the evidence that `Counter` is in `R`, `Rest` the row without it; the handler is generic
in both, so it handles a `Counter` anywhere in any row.

## 4. A handler that resumes twice

A general handler (`Handler`) gets the rest of the program as `k`, a function — and may call it any number of times.
Each call runs the rest anew:

```scala
enum Coin[X]:
  case Flip extends Coin[Boolean]

def flip[R[_]: Has[Coin]]: Boolean ! R = perform[Coin](Coin.Flip)

/** every way the program can go: `k` resumed with `true`, then with `false` */
def outcomes[R[_], Rest[_], A](p: A ! R)(using Split[Coin, R, Rest]): List[A] ! Rest =
  handle(p, new Handler[Coin, Rest, A, List[A]]:
    def ret(a: A) = pure(List(a))
    def op[X](op: Coin[X], k: X => List[A] ! Rest) = op match
      case Coin.Flip => k(true).flatMap(t => k(false).map(t ++ _)))

val twoFlips = for a <- flip[Coin]; b <- flip[Coin] yield (a, b)
assert(run(outcomes(twoFlips)) == List((true, true), (true, false), (false, true), (false, false)))
```

## 5. Code generic in its row

A program can ask for effects without naming its row (`Has`, the context bound, is `In` evidence). Pass the evidence
on to a recursion, and the row is named once, outside:

```scala
def sum[R[_]: {Has[Reader % Int], Has[State % Int]}](n: Int): Unit ! R =
  if n == 0 then pure(())
  else ask[Int, R].flatMap(x => modify[Int, R](_ + x)).andThen(sum[R](n - 1))

assert(run(State.run(0)(Reader.run(2)(sum[Counting](100))))._1 == 200)
```

A program of a smaller row goes into a bigger one with `into`:

```scala
val small: Int ! (State % Int) = get[Int, State % Int]
val bigger: Int ! (Reader % Int + State % Int) = small.into[Reader % Int + State % Int]
assert(run(Reader.run(0)(State.run(7)(bigger))) == (7, 7))
```

## 6. Choice: every way, or the first

`Choose` is the standard non-determinism: `among(xs)` one of the values, `fail` none; `Choose.all`
resumes for each choice, `Choose.first` gives the first way with no `fail`:

```scala
def pairs[R[_]: Has[Choose]]: (Int, Int) ! R =
  for
    a <- among[Int, R](List(1, 2, 3))
    b <- among[Int, R](List(1, 2, 3))
    _ <- guard[R](a < b)
  yield (a, b)

assert(run(Choose.all(pairs[Choose])) == List((1, 2), (1, 3), (2, 3)))
assert(run(Choose.first(pairs[Choose])) == Some((1, 2)))
```

Where a state meets a choice, the order of their handlers decides whether each branch has its own state:

```scala
type CS = Choose + State % Int
val counted = among[Int, CS](List(1, 2, 3)).flatMap(x => modify[Int, CS](_ + 1).as(x))

assert(run(Choose.all(State.run(0)(counted))) == List((1, 1), (1, 2), (1, 3)))      // each branch its own state
assert(run(State.run(0)(Choose.all(counted))) == (3, List(1, 2, 3)))                // one state through every branch
```

## 7. Errors

`raise` ends the program with an error; `Throws.run` gives the value or what was raised, `Throws.recover` a
program in its place:

```scala
def half[R[_]: Has[Throws % String]](n: Int): Int ! R =
  if n % 2 == 0 then pure(n / 2) else raise[String, R, Int](s"$n is odd")

assert(run(Throws.run[String](half[Throws % String](10))) == Right(5))
assert(run(Throws.run[String](half[Throws % String](7))) == Left("7 is odd"))
assert(run(Throws.recover[String](e => pure(e.length))(half[Throws % String](7))) == 8)
```

## 8. Generators

`emit` (`Emit.send`) gives a value out and goes on; `Emit.collect` gathers them, `Emit.take(n)` stops after `n`,
`Emit.each` gives each to a consumer of its own effects:

```scala
def countdown[R[_]: Has[Emit % Int]](n: Int): Unit ! R =
  if n == 0 then pure(()) else emit[Int, R](n).andThen(countdown[R](n - 1))

assert(run(Emit.collect[Int](countdown[Emit % Int](3))) == (List(3, 2, 1), ()))
assert(run(Emit.take[Int](2)(countdown[Emit % Int](100))) == (List(100, 99), None))
```

## 9. Interception

`intercept` gives an effect's operations to a handler that keeps the row: its clause runs outside, so the operation
it passes on goes to the effect's real handler. `observe` and `around` are built on it:

```scala
type SW = Writer % String + State % Int
val program = put[Int, SW](1).andThen(get[Int, SW]).flatMap(n => put[Int, SW](n * 10))

val traced = observe[State % Int, SW, Unit](program)(new Observe[State % Int, SW]:
  def apply[X](op: State[Int, X]): Unit ! SW = op match
    case State.Get(_) => tell[String, SW]("get")
    case State.Put(s, _) => tell[String, SW](s"put $s"))

assert(run(Writer.run[String](State.run(0)(traced))) == (List("put 1", "get", "put 10"), (10, ())))
```

## 10. Delimited control

`shift` takes the rest of the program up to the nearest `reset` as `k`; here resumed twice — `1 + (1 + 10)`:

```scala
type C = Control % (Int ! Pure)
assert(run(reset[Int, Pure](shift[Int, Int, Pure, Pure + C](k => k(10).flatMap(k)).map(_ + 1))) == 12)
```

## 11. One program, any implementation

A program written over the interface runs on whichever implementation is given. With `Tagless`, the program is
even a value of no implementation — made once, run on each:

```scala
object TaglessTour:
  import okay.effects.tagless.Tagless.*
  import okay.effects.tagless.Tagless.std.*

  type RS = Reader % Int + State % Int
  /** a program of no implementation: a value */
  val made: (Int, Int) ! Pure = State.run(0)(Reader.run(5)(ask[Int, RS].flatMap(x => modify[Int, RS](_ + x))
    .andThen(get[Int, RS])))

  /** run on the implementation in the implicit context */
  val onMachine = run(made)(using okay.effects.machine.Machine)
  val onFree = run(made)(using okay.effects.free.Free)

assert(TaglessTour.onMachine == (5, 5) && TaglessTour.onFree == (5, 5))
```

## Where next

- The [guide](README.md#the-guide), each part of the interface in depth.
- The [modules](README.md#the-modules): streams, asynchrony, concurrency, resources, I/O as effects, probability,
  parsing, logic programming, transactions, retries, interop with cats and ZIO.
