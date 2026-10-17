# Handlers

A handler gives an effect its meaning: it takes a program whose row has the effect and gives one whose row has not.
`handle(p, h)` takes the effect from wherever it is in the row, by `Split[E, R, Rest]`; the result is over `Rest`.

Five shapes, from the most general to the most specialised. An implementation may run a specialised shape faster
(an answer in place needs no `k` at all), so use the narrowest that says what you mean.

| Shape | Clauses | `k` | for |
|---|---|---|---|
| `Handler[E, R, A, B]` | `ret(a)`, `op(op, k)` | the rest, resumed any number of times | choice, generators, search, schedulers |
| `TailHandler[E, R, A, B]` | `ret(a)`, `answer(op): X` | resumed once, at once, with the answer | `Reader`, environments, services |
| `AbortHandler[E, R, A, B]` | `ret(a)`, `abort(op)` | never resumed: the clause is the result | `Error`, `Abort` |
| `StatefulTailHandler[E, S, R, A, B]` | `ret(s, a)`, `answer(cell, op): X` | resumed once, a state in a `Cell` | `State`, `Writer`, counters |
| `StatefulHandler[E, S, R, A, B]` | `ret(s, a)`, `op(s, op, k)` | resumed with the answer and the next state | `take`, schedulers with state |

`R` in a shape is the row OUTSIDE the handler; `A` the program's value, `B` the handler's.

```scala
import okay.effects.machine.Machine.*
import okay.effects.machine.Machine.std.*

enum Ask[X]:
  case Name extends Ask[String]
  case Age extends Ask[Int]

def name[R[_]: Has[Ask]]: String ! R = perform[Ask](Ask.Name)
def age[R[_]: Has[Ask]]: Int ! R = perform[Ask](Ask.Age)
def who[R[_]: Has[Ask]]: String ! R = for n <- name[R]; a <- age[R] yield s"$n, $a"
```

## Answered in place: `TailHandler`

```scala
def answering[R[_], Rest[_], A](p: A ! R)(using Split[Ask, R, Rest]): A ! Rest =
  handle(p, new TailHandler[Ask, Rest, A, A]:
    def ret(a: A) = pure(a)
    def answer[X](op: Ask[X]): X = op match
      case Ask.Name => "Ann"
      case Ask.Age => 30)

assert(run(answering(who[Ask])) == "Ann, 30")
```

`answer` returns the operation's answer itself — the program goes on at once with it.

## General: `Handler`

The clause gets `k`, the rest of the program under this handler, and gives the handler's result. `k` may be called
twice (every way), once later (a scheduler), or never (an abort):

```scala
def bothAges[R[_], Rest[_]](p: String ! R)(using Split[Ask, R, Rest]): List[String] ! Rest =
  handle(p, new Handler[Ask, Rest, String, List[String]]:
    def ret(a: String) = pure(List(a))
    def op[X](op: Ask[X], k: X => List[String] ! Rest) = op match
      case Ask.Name => k("Bob")
      case Ask.Age => k(20).flatMap(a => k(40).map(a ++ _)))

assert(run(bothAges(who[Ask])) == List("Bob, 20", "Bob, 40"))
```

Handlers are DEEP: the rest `k` is still under the handler, so the operations it performs come to this handler again.

## Aborting: `AbortHandler`

The clause's program is the handler's result; the rest is dropped:

```scala
enum Stop[X]:
  case Now(reason: String) extends Stop[Nothing]

def stop[R[_]: Has[Stop]](reason: String): Nothing ! R = perform[Stop](Stop.Now(reason))

def stopping[R[_], Rest[_], A](p: A ! R)(using Split[Stop, R, Rest]): Either[String, A] ! Rest =
  handle(p, new AbortHandler[Stop, Rest, A, Either[String, A]]:
    def ret(a: A) = pure(Right(a))
    def abort[X](op: Stop[X]) = op match
      case Stop.Now(reason) => pure(Left(reason)))

assert(run(stopping(stop[Stop]("enough").map(_ => 1))) == Left("enough"))
```

## With a state: `StatefulTailHandler`

A state of the handler's own, read and written in place through a `Cell`; `ret` gets the final state:

```scala
def counted[R[_], Rest[_], A](p: A ! R)(using Split[Ask, R, Rest]): (Int, A) ! Rest =
  handle(p, 0, new StatefulTailHandler[Ask, Int, Rest, A, (Int, A)]:
    def ret(n: Int, a: A) = pure((n, a))
    def answer[X](cell: Cell[Int], op: Ask[X]): X =
      cell.set(cell.get + 1)
      op match
        case Ask.Name => "Cy"
        case Ask.Age => cell.get)

assert(run(counted(who[Ask])) == (2, "Cy, 2"))
```

A `k` taken by another handler and resumed twice resumes the state of ITS time: each resumption its own.

## General with a state: `StatefulHandler`

The clause gets the state of its time, and `k` takes the answer and the state to go on from:

```scala
/** at most `n` answers, then the default */
def limited[R[_], Rest[_], A](n: Int)(p: A ! R)(using Split[Ask, R, Rest]): A ! Rest =
  handle(p, n, new StatefulHandler[Ask, Int, Rest, A, A]:
    def ret(s: Int, a: A) = pure(a)
    def op[X](s: Int, op: Ask[X], k: (X, Int) => A ! Rest) = op match
      case Ask.Name => k(if s > 0 then "Dee" else "?", s - 1)
      case Ask.Age => k(if s > 0 then 50 else 0, s - 1))

assert(run(limited(1)(who[Ask])) == "Dee, 0")
```

## The last place: `handleLast`

`handle` takes the effect from any place by `Split`. `handleLast(p, h)` handles the last effect of `R + E` — what an
implementation provides; `handle` is `split` then `handleLast`, unless the implementation takes it from its place at
once (the machine and `Free` do).

## Interception

A handler removes its effect from the row. `intercept` keeps it: see [interception](interception.md).
