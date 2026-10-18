# stream — lazy streams

`okay-effects-stream`, `okay.effects.stream` — `import okay.effects.stream.*`, the module on the machine. A `Stream[O, R]` gives elements `O` one at a time, each PULL a program
of the row `R` — so its elements may be made by effects, and it is lazy: nothing is pulled until a sink asks, and an
endless stream is taken as far as it is asked. A stream is a value: pulled twice, it runs twice.

| | |
|---|---|
| sources | `emits(xs)`, `range(a, b)`, `iterate(x)(f)`, `unfold(s)(f)`, `eval(p)`, `repeatEval(p)`, `empty` |
| transformations | `map`, `evalMap(f)`, `filter`, `take`, `drop`, `takeWhile`, `dropWhile`, `scan`, `zip`, `zipWithIndex`, `interleave`, `++`, `flatMap` |
| merging | `merge(a, b, ...)` or `merge(list)`: one element of each in turn |
| sinks | `toList`, `fold(z)(f)`, `foreach(f)`, `drain`, `headOption` |
| generators | `fromGenerator(p)`: a program that `emit`s as a stream; `s.emitAll`: a stream as a program that `emit`s |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.stream.*

type P = Pure
assert(run(range[P](0, 10).map(_ * 3).filter(_ % 2 == 0).drop(1).take(3).toList) == List(6, 12, 18))
assert(run(iterate[Int, P](1)(_ * 2).take(5).toList) == List(1, 2, 4, 8, 16))
assert(run(range[P](1, 5).scan(0)(_ + _).toList) == List(0, 1, 3, 6, 10))
assert(run(emits[String, P](List("a", "b")).zip(iterate[Int, P](1)(_ + 1)).toList) == List(("a", 1), ("b", 2)))
```

## Merge, in turn

One element of each stream in turn; a stream that ends leaves the turn. No channel, no buffer — each pull is the
next stream's own:

```scala
val merged = merge[String, P](emits(List("a1", "a2", "a3")), emits(List("b1")), emits(List("c1", "c2")))
assert(run(merged.toList) == List("a1", "b1", "c1", "a2", "c2", "a3"))
assert(run(merge[String, P](List(emits(List("x")), emits(List("y")))).toList) == List("x", "y"))      // or of a list
```

For streams that wait, merged as their elements come, see [async](async.md#streams-merged-as-they-come).

## Effects in a stream

```scala
type St = State % Int
val counted = repeatEval[Int, St](modify[Int, St](_ + 1).andThen(get[Int, St])).take(3).toList
assert(run(state(0)(counted)) == (3, List(1, 2, 3)))     // pulled three times, no more

type SW = Writer % String + State % Int
val totals = range[SW](1, 4).evalMap(n => modify[Int, SW](_ + n).andThen(get[Int, SW])).foreach(t => tell[String, SW](s"total $t"))
assert(run(writer[String](state(0)(totals))) == (List("total 1", "total 3", "total 6"), (6, ())))
```

## Generators

```scala
type Y = Yield % Int
def naturals(n: Int): Unit ! Y = emit[Int, Y](n).andThen(naturals(n + 1))

assert(run(fromGenerator[Int](naturals(0)).filter(_ % 3 == 0).take(4).toList) == List(0, 3, 6, 9))
assert(run(collect[Int](range[P](0, 3).emitAll[Y])) == (List(0, 1, 2), ()))
```

The generator runs only as far as it is pulled. Constant stack: 100 000 elements through `map`, `filter`, `fold`.
