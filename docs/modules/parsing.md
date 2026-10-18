# parsing — parsers of two standard effects

`okay-effects-parsing`, `okay.effects.parsing` — `import okay.effects.parsing.*`, the module on the machine. A parser is a program of two standard effects: the input left, a
`State[String]`, and the alternatives, `Choose`. `parse(input)(p)` runs it with the state inside the choice — each
alternative tried from the input of its time — and gives the first way that parses all of the input.

| | |
|---|---|
| parsers | `item`, `satisfy(f)`, `char(c)`, `string(s)`, `eof` |
| combinators | `orElse(p, q)`, `many(p)`, `many1(p)`; and any program's `map`, `flatMap`, `for` |
| run | `parse(input)(p)`: `Option[A]` |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.parsing.*

type P = Choose + Input

def number: Int ! P = many1[Char, P](satisfy[P](_.isDigit)).map(_.mkString.toInt)
def factor: Int ! P = orElse[Int, P](number, char[P]('(').andThen(expr).flatMap(e => char[P](')').as(e)))
def term: Int ! P = factor.flatMap(f => many[Int, P](char[P]('*').andThen(factor)).map(fs => (f :: fs).product))
def expr: Int ! P = term.flatMap(t => many[Int, P](char[P]('+').andThen(term)).map(ts => (t :: ts).sum))

assert(run(parse("2+3*(4+1)")(expr)) == Some(17))
assert(run(parse("2+")(expr)) == None)
assert(run(parse("letter")(orElse[String, P](string[P]("let"), string[P]("letter")))) == Some("letter"))
```

Backtracking is the choice's: `orElse` tries `p`, and if the whole parse fails that way, `q` from the same input. A
parser that loops on no input (`many` of a parser that succeeds consuming nothing) does not end.
