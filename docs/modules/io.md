# io — the console, the clock, the environment

`okay-effects-io`, `okay.effects.world` — `import okay.effects.world.*`, the module on the machine. The world as effects: a program names what it touches in its row, and its
handler says how — the world's, or a pure one for tests. The same program under either.

| Effect | Operations | The world's handler | A pure handler |
|---|---|---|---|
| `Console` | `readLine` (`Option[String]`), `printLine(s)` | `realConsole` | `console(inputs)`: the lines written, with the value |
| `Clock` | `now` (milliseconds) | `systemClock` | `clock(start, step)` |
| `Env` | `getEnv(name)` | `systemEnv` | `env(vars)` |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.world.*

type App = Console + Clock + Env

def greet: Int ! App =
  for
    g     <- getEnv[App]("GREETING")
    _     <- printLine[App]("name?")
    name  <- readLine[App]
    t0    <- now[App]
    t1    <- now[App]
    _     <- printLine[App](s"${g.getOrElse("hi")}, ${name.getOrElse("nobody")}")
  yield (t1 - t0).toInt

assert(run(console(List("Ann"))(clock(1000, 5)(env(Map("GREETING" -> "hello"))(greet)))) == (List("name?", "hello, Ann"), 5))
assert(run(systemClock(now[Clock])) > 0)
```

The world's handlers do their operation where it is answered, in place: `realConsole` reads standard input and
prints, `systemClock` reads the system's time, `systemEnv` the process's environment (empty on Scala.js).
