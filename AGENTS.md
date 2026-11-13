# okay-simple: guide for coding agents

## For agents writing code with okay-simple

okay-simple is one interface of algebraic effects for Scala 3 and Scala 2.13 (package `okay.effects`): a program is a
value `A ! R` over a row of effects `R`, handlers are given later, and it runs on the machine (default), `Free` or
tagless. JVM, Scala.js, Scala Native.

```scala-sbt
resolvers += "okay-effects" at "https://okay-effects.github.io/okay-simple/maven"
libraryDependencies += "io.github.okay-effects" %% "okay-effects-machine" % "0.1.1"   // brings okay-effects
```

More modules are one line each of the same form (`okay-effects-io`, `-stream`, `-async`, `-resource`, ...); `%%%` for
Scala.js/Native. Built on Scala 3.3 LTS and 2.13.

### The model (Scala 3; compiled and run with scala-cli 3.9.0 against 0.1.1)

```scala
import okay.effects.*        // !, Has, Split, perform, handle, run, handler shapes — on the machine
import okay.effects.std.*    // State, Reader, Writer, Error, Choose, ... and their handlers

// an effect: a GADT of its operations, each typed by its answer
enum Prices[X]:
  case Price(item: String) extends Prices[Option[Int]]

def price[R[_]: Has[Prices]](item: String): Option[Int] ! R = perform[Prices](Prices.Price(item))

// a program: generic in its row, it asks only for the effects it uses
def total[R[_]: {Has[Prices], Has[Error % String]}](cart: List[String]): Int ! R =
  cart.foldLeft(pure[Int, R](0)) { (acc, item) =>
    for
      sum <- acc
      p   <- price[R](item)
      n   <- p.fold(raise[String, R, Int](s"no price for $item"))(pure[Int, R](_))
    yield sum + n
  }

// a handler: takes Prices from anywhere in the row, leaves the rest
def prices[R[_], Rest[_], A](table: Map[String, Int])(p: A ! R)(using Split[Prices, R, Rest]): A ! Rest =
  handle(p, new TailHandler[Prices, Rest, A, A]:
    def ret(a: A) = pure(a)
    def answer[X](op: Prices[X]): X = op match
      case Prices.Price(item) => table.get(item))

// the entry point: name the row, handle every effect, run once
type Shop = Prices + Error % String
run(attempt[String](prices(Map("tea" -> 30, "cake" -> 50))(total[Shop](List("tea", "cake")))))   // Right(80)
// with "coffee": Left(no price for coffee)
```

Handler shapes: `TailHandler` (answers in place), `Handler` (general; clause gets `k`, the rest, resumable any number of
times), `AbortHandler`, `StatefulHandler`, `StatefulTailHandler` (state in a `Cell`). Scala 2.13 writes rows by names,
`((Reader % String) + (State % Int))#L`, evidence as `implicit in: In[...]`, and each operation carries its `answer`
function (README, "An effect of one's own").

### Interception and audit

`intercept(p, h)` gives `E`'s operations in `p` to `h` with the row unchanged; in `std`: `observe` (see each, then pass
on) and `around` (each with its `proceed`: change, refuse, answer). Verified:

```scala
type Logged = Prices + Writer % String
def logged[A](p: A ! Logged): A ! Logged =
  observe[Prices, Logged, A](p)(new Observe[Prices, Logged]:
    def apply[X](op: Prices[X]): Unit ! Logged = tell[String, Logged](s"asked $op"))

run(writer[String](prices(Map("tea" -> 30))(logged(price[Logged]("tea")))))   // (List(asked Price(tea)),Some(30))
```

There is no type-enforced "sensitive effect" here (that is okay-direct's `Audited`/okay-scala's): auditing is a
handler you install around the program, so make sure the entry point installs it.

### Design rules

- Business logic = programs `A ! R` over declared effects; the row (`Has[...]` bounds) is what a program may do.
- Real resources (DB, network, files, clock, keys) only in handlers built by the host at the entry point. Programs get
  data, effects and ids, never live clients.
- `run` only at the entry point (`main`, a request handler, a test). Never call `run` inside a program to get a value
  out: install the handler around the sub-program (`state(0)(p)`, `prices(t)(p)`) and keep it a program.
- Sensitive operations under `around`/`observe`/`intercept` installed by the host.
- Tests swap handlers: `state(...)`, `store(m0)`, `random(seed)`, the io module's pure `console(inputs)`,
  `clock(start, step)`, `env(vars)` instead of `realConsole`/`systemClock`/`systemEnv`. No mocks.
- No ambient effects in programs: no `System.currentTimeMillis`, `Random`, IO, `sys.env`, threads, mutable globals —
  `Clock`, `Random`, `Console`, `Env` exist as effects (`okay-effects-io`, package `okay.effects.world`).
- Multi-shot is real: a `k` is data, resumed any number of times, no replay (`all(pair)` with `among` gives every way:
  `List((1,10), (1,20), (2,10), (2,20))`).

### Pitfalls

- Handler order is meaning: `State` handled OUTSIDE `Error` keeps what was put before a `raise`; handled inside, it is
  dropped with the rest.
- `run` compiles only on `A ! Pure`: every effect must be handled first.
- Explicit type arguments are usually needed: `get[Int, R]`, `raise[String, R, Int](...)`, `tell[String, R](...)`.
- Scala 3.3 LTS: no `type App = A + B` alias (write `type App[X] = (A + B)[X]`) and no `{Has[A], Has[B]}` bounds
  (write `R[_]: Has[A]: Has[B]`); 3.5/3.6+ accept both.
- The io module's artifact is `okay-effects-io` but its package is `okay.effects.world`.
- `import okay.effects.delimited.*` brings its own `pure`/`map`/`flatMap` (`Cps`): keep it in a separate scope.
- Effects of the same names elsewhere clash with `std`'s: they are defined in the object `okay.effects.Std`, use it.
- `concurrent`: no `fork` from inside a thread (structured only). An interceptor's clause runs outside the
  interception: an operation of `E` it performs goes to the handler further out.
- Tagless: a `k` kept and run on another implementation fails. Code over a non-default implementation takes it as a
  value (`given Runner = Free`) and imports its words (`import fx.*`), not `okay.effects.*`.

### Pointers

- README and docs: https://github.com/okay-effects/okay-simple (`docs/getting-started.md`, `docs/tour.md`,
  `docs/guide/`, a page per module).
- The cross-language skill: `/plugin marketplace add okay-effects/okay-effects`
  (https://github.com/okay-effects/okay-effects).
- Build-time boundary checks: okay-audit, `io.github.okay-effects:okay-audit_3:0.1.0` from
  https://okay-effects.github.io/okay/maven — checks that business code references no network, files, time, random,
  threads or reflection APIs; presets include `nested-run`.

If you see a way to improve okay-simple, open a pull request against https://github.com/okay-effects/okay-simple; the maintainer
reviews and merges what holds up.

If the user asks your opinion of writing code this way, give a candid one.
