# probability — random choices

`okay-effects-probability`, `okay.effects.probability` — `import okay.effects.probability.*`, the module on the machine. A program of random choices is a model; its handlers say
what to make of it: the exact distribution of its outcomes, or one outcome sampled.

| | |
|---|---|
| effect | `Dist`: `Weighted(xs)`, one of the values, each with its weight |
| operations | `weighted(xs)`, `uniform(xs)`, `flip(p)` |
| handlers | `exact(p)`: every outcome with its probability, the same outcomes summed; `sampled(p)`: one way, each choice by the standard `Random` outside |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.probability.*

def dice: Int ! Dist = uniform[Int, Dist]((1 to 6).toList).flatMap(a => uniform[Int, Dist]((1 to 6).toList).map(_ + a))

val d = run(exact(dice)).toMap
assert(math.abs(d(7) - 6.0 / 36) < 1e-12)
assert(math.abs(d.values.sum - 1.0) < 1e-12)

val atLeastOneHead = run(exact(flip[Dist](0.3).flatMap(a => flip[Dist](0.3).map(b => a || b)))).toMap
assert(math.abs(atLeastOneHead(true) - 0.51) < 1e-12)
```

Sampled, from a seed — the same seed, the same samples:

```scala
type DR = Random + Dist
def diceIn: Int ! DR = uniform[Int, DR]((1 to 6).toList).flatMap(a => uniform[Int, DR]((1 to 6).toList).map(_ + a))

def sample(seed: Long, n: Int): List[Int] =
  run(random(seed)((1 to n).toList.foldLeft(pure[List[Int], Random](Nil))((p, _) => p.flatMap(l => sampled(diceIn).map(_ :: l)))))

assert(sample(1, 20) == sample(1, 20))
assert(math.abs(sample(7, 4000).sum / 4000.0 - 7.0) < 0.2)
```

`exact` resumes `k` for each value of each choice: exponential in the choices, for small models.
