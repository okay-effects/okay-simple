# Interception

`intercept(p, h)` gives `E`'s operations in `p` to the general handler `h` — with `k`, the rest of `p`, still
intercepted — and keeps the row: the result is over `R`, `E` still in it. The clause runs outside the
interception, so an operation of `E` it performs goes to `E`'s handler further out. So an interceptor can

- observe an operation and pass it on (an audit trail, a count, a trace),
- change it, or perform another in its place,
- refuse it, or answer it here, never passing it on.

```scala
import okay.effects.machine.Machine.*
import okay.effects.machine.Machine.std.*

type Bank = Writer % String + State % Int

def deposit(n: Int): Unit ! Bank = modify[Int, Bank](_ + n)
val day: Int ! Bank = deposit(10).andThen(deposit(5)).andThen(get[Int, Bank])
```

## Observe

`observe(p)(f)`: each operation seen by `f`, a program of the row, then passed on as it was:

```scala
val audited = observe[State % Int, Bank, Int](day)(new Observe[State % Int, Bank]:
  def apply[X](op: State[Int, X]): Unit ! Bank = op match
    case State.Get(_) => tell[String, Bank]("read")
    case State.Put(s, _) => tell[String, Bank](s"wrote $s"))

assert(run(writer[String](state(0)(audited))) == (List("read", "wrote 10", "read", "wrote 15", "read"), (15, 15)))
```

(`modify` is a `get` and a `put`.)

## Around

`around(p)(f)`: `f` gets the operation and `proceed`, the program passing it on — and gives the program in its
place:

```scala
/** deposits doubled: each `put` given a value of its own */
val doubled = around[State % Int, Bank, Int](day)(new Around[State % Int, Bank]:
  def apply[X](op: State[Int, X], proceed: X ! Bank): X ! Bank = op match
    case State.Put(s, answer) => get[Int, Bank].flatMap(old => put[Int, Bank](old + 2 * (s - old))).map(answer)
    case _ => proceed)

assert(run(writer[String](state(0)(doubled)))._2 == (30, 30))
```

## A general interceptor

`intercept` itself takes a `Handler` whose row is the row of `p`:

```scala
/** reads answered here, never passed on: a fixed balance seen */
val frozen = intercept[State % Int, Bank, Int, Int](day, new Handler[State % Int, Bank, Int, Int]:
  def ret(a: Int) = pure(a)
  def op[X](op: State[Int, X], k: X => Int ! Bank) = op match
    case State.Get(answer) => k(answer(100))
    case State.Put(s, answer) => put[Int, Bank](s).flatMap(_ => k(answer(()))))

assert(run(writer[String](state(0)(frozen)))._2 == (105, 100))
```

## Nesting

Two interceptors of one effect: the inner sees an operation first, and what it passes on reaches the outer.

## How it works

The interface's primitive is `interposed(p)`, the implementation's: the program in `R + E`, its `E` moved to a new
last place, its other effects where they are. `intercept` is `handleLast` of it. The machine and `Free` do it by one
renaming of places (in Scala 3 typed: `Places.interposing`); okay-kernel's bridge by its `remap`; okay-core's by its
observer; tagless on the implementation it is given.
