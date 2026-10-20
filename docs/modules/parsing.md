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

## JSON, a grammar of its own on the module

A grammar that decides by the next character, written with `orElse` and `many`; the choice backtracks where the
grammar does not need it, and a failed parse is `None` with no position — see BACKLOG.md, "parsing: errors with a
position" and "parsing: a committed choice". okay-face's `Json.parse` is hand-written for those two reasons and for
speed; its tests check that this grammar agrees with it.

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.parsing.*

sealed trait Json
case object JNull extends Json
final case class JBool(b: Boolean) extends Json
final case class JNum(n: BigDecimal) extends Json
final case class JStr(s: String) extends Json
final case class JArr(items: List[Json]) extends Json
final case class JObj(fields: List[(String, Json)]) extends Json

type J = Choose + Input

def ws: Unit ! J = many[Char, J](satisfy[J](_.isWhitespace)).as(())
def token(c: Char): Unit ! J = ws.andThen(char[J](c)).andThen(ws)
def sepBy[A](p: A ! J, sep: Char): List[A] ! J =
  orElse[List[A], J](p.flatMap(a => many[A, J](token(sep).andThen(p)).map(a :: _)), pure[List[A], J](Nil))
def digits: String ! J = many1[Char, J](satisfy[J](_.isDigit)).map(_.mkString)
def optional(p: String ! J): String ! J = orElse[String, J](p, pure[String, J](""))
def jnumber: Json ! J =
  for
    sign <- optional(char[J]('-').as("-"))
    int  <- digits
    frac <- optional(char[J]('.').andThen(digits).map("." + _))
    exp  <- optional(satisfy[J](c => c == 'e' || c == 'E').andThen(optional(satisfy[J](c => c == '+' || c == '-').map(_.toString))).flatMap(s => digits.map("e" + s + _)))
  yield JNum(BigDecimal(sign + int + frac + exp))
def hex: Char ! J = satisfy[J](c => c.isDigit || ('a' to 'f').contains(c.toLower))
def escape: Char ! J = char[J]('\\').andThen(item[J]).flatMap {
  case 'n' => pure[Char, J]('\n')
  case 't' => pure[Char, J]('\t')
  case 'r' => pure[Char, J]('\r')
  case 'b' => pure[Char, J]('\b')
  case 'f' => pure[Char, J]('\f')
  case 'u' => hex.flatMap(a => hex.flatMap(b => hex.flatMap(c => hex.map(d => Integer.parseInt(s"$a$b$c$d", 16).toChar))))
  case c => pure[Char, J](c)
}
def text: String ! J =
  char[J]('"').andThen(many[Char, J](orElse[Char, J](satisfy[J](c => c != '"' && c != '\\'), escape))).flatMap(cs => char[J]('"').as(cs.mkString))
def literal(word: String, j: Json): Json ! J = string[J](word).as(j)
def value: Json ! J = ws.andThen(orElse[Json, J](literal("null", JNull), orElse[Json, J](literal("true", JBool(true)),
  orElse[Json, J](literal("false", JBool(false)), orElse[Json, J](jnumber, orElse[Json, J](text.map(JStr(_)),
  orElse[Json, J](array, obj))))))).flatMap(j => ws.as(j))
def array: Json ! J = char[J]('[').andThen(sepBy(value, ',')).flatMap(items => token(']').as(JArr(items)))
def obj: Json ! J =
  char[J]('{').andThen(sepBy(ws.andThen(text).flatMap(k => token(':').andThen(value).map(k -> _)), ',')).flatMap(fs => token('}').as(JObj(fs)))

assert(run(parse("""{"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"yé"}, "e": []}""")(value)) ==
  Some(JObj(List("a" -> JArr(List(JNum(1), JNum(-250), JBool(true))), "b" -> JObj(List("c" -> JNull, "d" -> JStr("x\"yé"))), "e" -> JArr(Nil)))))
assert(run(parse("[1, 2")(value)) == None)
assert(run(parse("""{"a": }""")(value)) == None)
```
