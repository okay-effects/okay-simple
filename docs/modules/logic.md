# logic — logic programming

`okay-effects-logic`, `okay.effects.logic` — `import okay.effects.logic.*`, the module on the machine. Terms, logic variables, unification — of standard effects: variables
`fresh` (`Fresh`), the bindings a `State`, the alternatives `Choose`. A relation runs either way. And a fair search,
breadth-first, for searches with ways that never end.

| | |
|---|---|
| terms | `Term.Var(id)`, `Term.Atom(name)`, `Term.Pair(h, t)`, `Term.empty`, `Term.list(ts*)` |
| words | `variable`, `unify(a, b)`, `either(p, q)`, `resolved(t)` |
| run | `solutions(p)`: every answer, depth-first; `fair(n)(p)`: the first `n`, breadth-first |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.logic.*
import okay.effects.logic.Term.*

type L = Choose + Fresh + Bindings

/** `out` is `l` and then `s` */
def appendo(l: Term, s: Term, out: Term): Unit ! L =
  either[Unit, L](
    unify[L](l, Term.empty).andThen(unify[L](s, out)),
    for
      h   <- variable[L]
      t   <- variable[L]
      res <- variable[L]
      _   <- unify[L](l, Pair(h, t))
      _   <- unify[L](out, Pair(h, res))
      _   <- appendo(t, s, res)
    yield ())

val (one, two, three) = (Atom("1"), Atom("2"), Atom("3"))

val forwards = variable[L].flatMap(out => appendo(list(one), list(two, three), out).andThen(resolved[L](out)))
assert(run(solutions(forwards)) == List(list(one, two, three)))

val backwards = for
  l <- variable[L]
  s <- variable[L]
  _ <- appendo(l, s, list(one, two, three))
  t <- resolved[L](Pair(l, s))
yield t
assert(run(solutions(backwards)).size == 4)            // every way to split the list in two
```

A fair search finds what a depth-first one never would:

```scala
def nats(n: Int): Int ! Choose = either[Int, Choose](nats(n + 1), pure(n))
assert(run(fair(3)(either[Int, Choose](nats(100), pure(-1)))) == List(-1, 100, 101))
```

Unification with the occurs check; `either` takes both ways by name, so a relation may recur in them.
