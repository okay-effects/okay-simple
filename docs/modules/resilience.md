# resilience — retries and fallbacks

`okay-effects-resilience`, `okay.effects.resilience` — `import okay.effects.resilience.*`, the module on the machine. A program that may raise `E` run again.

| | |
|---|---|
| `retry[E](n)(p)` | `p`, and up to `n` more times while it raises: `Either[E, A]`, the last error |
| `retryIf[E](n)(worth)(p)` | the same, only while what was raised is `worth` it |
| `fallback[E](p)(q)` | `q(e)` where `p` raised `e` |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.resilience.*

type T = State % Int + Error % String

/** fails until it has been tried `n` times, the tries counted in a state outside */
def flaky(n: Int): String ! T =
  modify[Int, T](_ + 1).andThen(get[Int, T]).flatMap(tries => if tries < n then raise[String, T, String](s"try $tries") else pure("ok"))

assert(run(state(0)(retry[String](5)(flaky(3)))) == (3, Right("ok")))
assert(run(state(0)(retry[String](1)(flaky(3)))) == (2, Left("try 2")))
assert(run(state(0)(fallback[String](flaky(9))(e => pure(s"after $e")))) == (1, "after try 1"))
```

Each run of `p` starts anew; what it did in effects outside (a state, what was told) stays done. A timeout is the
[async module's](async.md).
