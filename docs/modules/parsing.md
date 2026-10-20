# parsing — parsers of three effects

`okay-effects-parsing`, `okay.effects.parsing` — `import okay.effects.parsing.*`, the module on the machine. A parser is
a program of three effects: the `Input`, a cursor over characters; the `Expect`ations, what a parser wanted where it
failed; and the alternatives, `Choose`. `parse(text)(p)` runs it: the input handler INSIDE the choice, so each
alternative goes on from the input of its time; the expectations OUTSIDE it, so the furthest failure survives
backtracking and is the error reported, with its line and column and what was expected there.

A parser that decides by the next character — `byNext`, `whileNext`, `takeWhile` — makes no choice point: it runs as a
straight line of operations answered in place, on the heap, so a text nested a hundred thousand deep is parsed, not a
stack overflow. `orElse` and `many` backtrack by the choice, where a grammar needs it.

| | |
|---|---|
| the input | `peek`, `take`, `takeWhile(ok)`, `takeIf(s)`, `offset`, `skipWhile(ok)`, `spaces` — one operation per token |
| parsers | `item`, `satisfy(ok, what)`, `char(c)`, `string(s)`, `takeWhile1(ok, what)`, `eof`; `fail(what)` |
| by the next character | `byNext(what)(cases)`, `whileNext(ok)(p)` — no choice point |
| alternatives | `orElse(p, q)`, `many(p)`, `many1(p)`, `optional(p)`, `sepBy(p, sep)`, `sepBy1(p, sep)` — backtracking |
| run | `parse(text)(p)`: `Either[ParseError, A]`; `parseStream(chunks)(p)` from a stream of chunks ([json](json.md) parses as values come) |
| handlers | `input(text)`, `inputFrom(chunks)`, `expectations` — to compose a run of one's own |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.parsing.*

type P = Choose + Expect + Input

def number: Int ! P = takeWhile1[P](_.isDigit, "a digit").map(_.toInt)
def factor: Int ! P = orElse[Int, P](number, char[P]('(').andThen(expr).flatMap(e => char[P](')').as(e)))
def term: Int ! P = factor.flatMap(f => many[Int, P](char[P]('*').andThen(factor)).map(fs => (f :: fs).product))
def expr: Int ! P = term.flatMap(t => many[Int, P](char[P]('+').andThen(term)).map(ts => (t :: ts).sum))

assert(run(parse("2+3*(4+1)")(expr)) == Right(17))
assert(run(parse("2+3)")(expr)).left.map(_.message) == Left("at line 1, column 4: expected '*' or '+' or end of input, found ')'"))
assert(run(parse("letter")(orElse[String, P](string[P]("let"), string[P]("letter")))) == Right("letter"))
assert(run(parse("(" * 100000 + "1" + ")" * 100000)(expr)) == Right(1))
```

Backtracking is the choice's: `orElse` tries `p`, and if the whole parse fails that way, `q` from the same input.
`satisfy` takes nothing when it fails, so a choice decided by one character leaves the input where it was. A parser
that loops on no input (`many` of a parser that succeeds consuming nothing) does not end.

## Decided by the next character

`byNext(what)(cases)` gives the parser for the next character and fails saying `what` for any other; `whileNext(ok)(p)`
repeats `p` while the next character is `ok`. No choice point is made: the operations are answered in place, and the
error is where the grammar says, not the furthest of alternatives tried.

```scala
def word: String ! P = byNext[String, P]("a fruit") {
  case 'a' => Some(string[P]("apple"))
  case 'b' => Some(string[P]("banana"))
  case _ => None
}
def words: List[String] ! P = whileNext[String, P](c => c == 'a' || c == 'b')(word.flatMap(w => skipWhile[P](_ == ' ').as(w)))

assert(run(parse("apple banana apple")(words)) == Right(List("apple", "banana", "apple")))
assert(run(parse("apricot")(words)).left.map(e => (e.offset, e.expected)) == Left((0L, List("'apple'"))))
```

## From a stream

`parseStream(chunks)(p)`: the input pulled from a stream of chunks as the parser needs it, so a text need not be in
memory. The chunks are a stream of the row outside the parser's three effects. The error says the offset; no line and
column, the text is not kept. A rolled-back alternative pulls a chunk again, so the source must remember what it gave
(`fromIterator` does); a parser decided by the next character never rolls back.

```scala
import okay.effects.stream.*

val chunks = fromIterator[String, Pure](Iterator("2+3*", "(4", "+1)"))
assert(run(parseStream(chunks)(expr)) == Right(17))
```

## The input in place

`input(text)` is a stateful handler answered in place: an index into the text, no copy of it. `takeWhile` and `takeIf`
are operations of the input, so a token costs one operation and the loop over its characters is the handler's. That is
what makes a parser on effects fast enough: measured in [json](json.md).

