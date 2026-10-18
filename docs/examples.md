# Examples

What the library is like, in the examples it has: the `examples/` module — programs written over the interface
alone, run on every implementation — and the most interesting of the examples in the modules' pages.

The excerpts below (```` ```scala excerpt ````) are from `examples/`, compiled and tested there (`sbt examples/test`) on
Scala 3.3 LTS — so written in its forms (see [Scala versions](getting-started.md#scala-versions));
the last block of the first part runs them, here, on two implementations.

## The `examples/` module

| File | What it shows |
|---|---|
| [`Basics3.scala`](../examples/src/main/scala-3/okay/effects/examples/Basics3.scala) | Scala 3: rows of type lambdas, requirements as context bounds, an effect of one's own an `enum`, a handler answering in place, a general one resuming `k` twice |
| [`Control3.scala`](../examples/src/main/scala-3/okay/effects/examples/Control3.scala) | generators (`collect`, `take`, `each`), a search (`first`), `recover`, `shift`/`reset`, the answer type changed |
| [`Basics2.scala`](../examples/src/main/scala-2/okay/effects/examples/Basics2.scala) | Scala 2: rows by names (`#L`), an effect's operations carrying their answers, a search |
| [`Shared.scala`](../examples/src/main/scala/okay/effects/examples/Shared.scala) | code written once for both compilers: an effect, its handler, a row by aliases |
| [`Mixed.scala`](../examples/mixed/src/main/scala/okay/effects/examples/Mixed.scala) | one application of two compilers: Scala 3 code using the effect, handler and program Scala 2.13 compiled |
| [`OnMachine.scala`](../examples/src/main/scala/okay/effects/examples/OnMachine.scala), [`OnFree.scala`](../examples/src/main/scala/okay/effects/examples/OnFree.scala) | all of them run on the machine, on `Free` — the same lines printed |

```sh
sbt "examples/runMain okay.effects.examples.OnMachine"
sbt "examples/runMain okay.effects.examples.OnFree"
```

```text
greeted       (1,(List(hello, Ann),0))
counted       (102,(2,2))
twoFlips      List((true,true), (true,false), (false,true), (false,false))
collected     (List(3, 2, 1),())
firstTwo      (List(5, 4),None)
summed        (10,())
pythagorean   Some((3,4,5))
halves        (5,8)
twice         12
greeting      hello, Ann
served        (List(1: Ann, 2: Bob),())
```

### A row handled in any order

A program asks for three effects at any place of its row, so it runs in any row that has them; each handler takes
its effect from wherever it is. The reader is handled first here, though it is the row's first:

```scala excerpt
def greet[R[_]: Has[Reader % String]: Has[Writer % String]: Has[State % Int]]: Int ! R =
  for
    name <- ask[String, R]
    _ <- tell[String, R](s"hello, $name")
    n <- get[Int, R]
    _ <- put[Int, R](n + 1)
  yield n

type App[X] = (Reader % String + Writer % String + State % Int)[X]   // Scala 3.3 LTS's form; 3.5 on: type App = ...
def greeted: (Int, (List[String], Int)) = run(state(0)(writer[String](reader("Ann")(greet[App]))))
```

### A handler resuming twice: every way a program can go

`k` is the rest of the program; a general handler may resume it any number of times. Resumed with `true` and then
with `false`, two flips give all four outcomes — multi-shot continuations, on every implementation:

```scala excerpt
enum Coin[X]:
  case Flip extends Coin[Boolean]

def flip[R[_]: Has[Coin]]: Boolean ! R = perform[Coin](Coin.Flip)

def outcomes[R[_], Rest[_], A](p: A ! R)(using Split[Coin, R, Rest]): List[A] ! Rest =
  handle(p, new Handler[Coin, Rest, A, List[A]]:
    def ret(a: A) = pure(List(a))
    def op[X](op: Coin[X], k: X => List[A] ! Rest) = op match
      case Coin.Flip => k(true).flatMap(t => k(false).map(t ++ _)))

def twoFlips = run(outcomes(for a <- flip[Coin]; b <- flip[Coin] yield (a, b)))   // (t,t), (t,f), (f,t), (f,f)
```

### A search

`among` chooses, `fail` goes back to the last choice, `first` keeps the first way through — a backtracking search in
a few lines:

```scala excerpt
def triple[R[_]: Has[Choose]]: (Int, Int, Int) ! R =
  for
    a <- among[Int, R]((1 to max).toList)
    b <- among[Int, R]((a to max).toList)
    c <- among[Int, R]((b to max).toList)
    t <- if a * a + b * b == c * c then pure[(Int, Int, Int), R]((a, b, c)) else fail[(Int, Int, Int), R]
  yield t
run(first(triple[Choose]))   // Some((3, 4, 5))
```

### Delimited control, the answer type changed

`shift` takes the rest up to its `reset` as a function. With `delimited` the `reset`'s answer may be of another
type than its body's: here the body answers a `String`, the `reset` a function waiting for the name:

```scala excerpt
val d = fx.delimited
val f = d.reset[String, String => String ! Pure, Pure](
  d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((name: String) => k(name))))("hello, " + _))
run(f.flatMap(g => g("Ann")))   // "hello, Ann"
```

### Written once for both compilers

An effect, its handler and a row written in the shared source, read the same by Scala 3 and Scala 2.13: the
operation carries how its answer is made (Scala 2 has no GADT inference for a method's type parameter), the row is
`Sum[R, E, X]`:

```scala excerpt
sealed trait Ticket[X]
object Ticket { final case class Next[X](answer: Int => X) extends Ticket[X] }

def tickets[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Ticket, R, Rest]): A ! Rest =
  handle[Ticket, Int, R, Rest, A, A](p, 0, new StatefulTailHandler[Ticket, Int, Rest, A, A] {
    def ret(n: Int, a: A) = pure(a)
    def answer[X](cell: Cell[Int], op: Ticket[X]): X = op match {
      case Ticket.Next(answer) => cell.set(cell.get + 1); answer(cell.get)
    }
  })

type Desk[X] = Sum[({ type L[x] = Sum[Void, Lines, x] })#L, Ticket, X]
```

### Two compilers, one application

Scala 3 code takes Scala 2.13's effect, handler and program as they are, in one row and one run (`Mixed.scala`):

```scala excerpt
val basics = Basics2[fx.type]()(using fx)       // compiled by Scala 2.13

def doubled[R[_]: Has[Basics2.Counter]#L: Has[(State % Int)#L]#L]: Int ! R =
  basics.tick[R].andThen(basics.peek[R]).flatMap(n => modify[Int, R](_ + 2 * n).as(n))

type Row[X] = (*[Basics2.Counter] + (State % Int))#L[X]
def both = run(state(0)(basics.counting(basics.tick[Row].andThen(doubled[Row]))))
```

### One program, any implementation

The examples take the implementation in the implicit context; given the machine or `Free`, the same values:

```scala
object ExamplesOn:
  import okay.effects.Runner
  import okay.effects.examples.{Basics3, Control3, Shared}

  def values(using Runner) =
    (Basics3().twoFlips, Basics3().greeted, Control3().pythagorean(20), Control3().twice, Control3().greeting,
      Shared().served(List("Ann", "Bob")))

  val machine = values(using okay.effects.machine.Machine)
  val free = values(using okay.effects.free.Free)

assert(ExamplesOn.machine == ExamplesOn.free)
assert(ExamplesOn.machine._3 == Some((3, 4, 5)) && ExamplesOn.machine._4 == 12)
assert(ExamplesOn.machine._6 == (List("1: Ann", "2: Bob"), ()))
```

## In the modules' pages

Each checked where it is; a taste of each here.

**A relation run both ways** — [logic](modules/logic.md). `appendo(l, s, out)`: `out` is `l` and then `s`. Given
`l` and `s` it makes `out`; given `out`, every way to split it:

```scala excerpt
val backwards = for
  l <- variable[L]
  s <- variable[L]
  _ <- appendo(l, s, list(one, two, three))
  t <- resolved[L](Pair(l, s))
yield t
run(solutions(backwards)).size   // 4
```

**An exact distribution** — [probability](modules/probability.md). The handler resumes the rest once per outcome,
each with its probability:

```scala excerpt
def dice: Int ! Dist = uniform[Int, Dist]((1 to 6).toList).flatMap(a => uniform[Int, Dist]((1 to 6).toList).map(_ + a))
run(exact(dice)).toMap(7)   // 6 / 36
```

**A parser of arithmetic** — [parsing](modules/parsing.md). A state of the input and a choice, backtracking:

```scala excerpt
def number: Int ! P = many1[Char, P](satisfy[P](_.isDigit)).map(_.mkString.toInt)
def factor: Int ! P = orElse[Int, P](number, char[P]('(').andThen(expr).flatMap(e => char[P](')').as(e)))
def term: Int ! P = factor.flatMap(f => many[Int, P](char[P]('*').andThen(factor)).map(fs => (f :: fs).product))
def expr: Int ! P = term.flatMap(t => many[Int, P](char[P]('+').andThen(term)).map(ts => (t :: ts).sum))
run(parse("2+3*(4+1)")(expr))   // Some(17)
```

**A transaction rolled back** — [transaction](modules/transaction.md). Writes go to a log, written to the store at
the end; an error raised inside drops the log, and the store is as it was:

```scala excerpt
run(store[String, Int](start)(attempt[String](atomically[String, Int](transfer("ann", "bob", 20)))))
  // (start, Left("insufficient"))
```

**Threads in turns** — [concurrent](modules/concurrent.md). Each thread a coroutine, `pass` giving the turn:

```scala excerpt
def worker(name: String, n: Int): Int ! T =
  if n == 0 then pure(0)
  else tell[String, T](s"$name$n").andThen(pass[Int, T]).andThen(worker(name, n - 1)).map(_ + 1)
run(writer[String](scheduled[Int](List(worker("a", 2), worker("b", 3)))))   // a2, b3, a1, b2, b1
```

**Streams merged as they come** — [async](modules/async.md): two streams of `Async`, the merged one giving whichever
element is ready first. **ZIO's own types as effects** — [interop](modules/interop.md#zios-own-types-as-effects): ZIOs
of different environments and errors in one program, made one ZIO of `Db & Log` and `DbErr | LogErr`.
