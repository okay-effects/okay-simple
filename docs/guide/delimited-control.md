# Delimited control

Two forms: `shift`/`reset` as a standard effect, the answer type fixed; and `delimited`, a type of its own, the
answer type changing.

## shift and reset, an effect

`Control % (B ! R)` is an effect; `reset(p)` its handler. `shift(f)` takes the rest of the program up to the nearest
`reset` as `k` — a function, resumable any number of times — and runs `f(k)` in its place; `f`'s value is the
`reset`'s:

```scala
import okay.effects.machine.Machine
import okay.effects.*
import okay.effects.std.*

type C = Control % (Int ! Pure)

/** `k` is "+ 1"; resumed twice: 1 + (1 + 10) */
assert(run(reset[Int, Pure](shift[Int, Int, Pure, Pure + C](k => k(10).flatMap(k)).map(_ + 1))) == 12)
/** `k` dropped: the `reset`'s value is `f`'s */
assert(run(reset[Int, Pure](shift[Int, Int, Pure, Pure + C](_ => pure(42)).map(_ + 1))) == 42)
```

## Changing the answer type: `delimited`

`fx.delimited` is `Delimited`: programs `Cps[A, B, C, R]` of value `A` whose `reset` answers `B` where it is, `C`
outside — so `shift` may change the answer type (Danvy and Filinski). Its words: `pure`, `map`, `flatMap`, `lift`
(a program of `R`), `shift`, `reset`.

```scala
val d = Machine.delimited
/** the body's answer a `String`; the `reset`'s a function from the name */
val f = d.reset[String, String => String ! Pure, Pure](
  d.map(d.shift[String, String, String => String ! Pure, Pure](k => pure((name: String) => k(name))))("hello, " + _))

assert(run(f.flatMap(g => g("Ann"))) == "hello, Ann")
```

On the machine, the default, `import okay.effects.delimited.*` gives these words — in a scope of their own, as their
`pure`, `map`, `flatMap` are `Cps`'s:

```scala
object WithDelimited:
  import okay.effects.delimited.*
  val greeting = reset[String, String => String ! Pure, Pure](
    map(shift[String, String, String => String ! Pure, Pure](k => okay.effects.pure((name: String) => k(name))))("hi, " + _))

assert(run(WithDelimited.greeting.flatMap(g => g("Bo"))) == "hi, Bo")
```

By default `Delimited.ByCps`, continuation-passing over the implementation's programs, in constant stack. okay-core's
bridge has it native, on okay-core's own `shift0` and `$`.
