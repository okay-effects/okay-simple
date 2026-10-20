package okay.effects.parsing.checks

import okay.effects.parsing.{Parsing}

import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ParsingChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val ps = Parsing()
  import ps._
  import ps.fx._

  type P[X] = Sum[({ type L[x] = Sum[Void, Std.Choose, x] })#L, Input, X]

  def number: Int ! P = many1[Char, P](satisfy[P](_.isDigit)).map(_.mkString.toInt)
  def factor: Int ! P = orElse[Int, P](number, char[P]('(').flatMap(_ => expr).flatMap(e => char[P](')').as(e)))
  def term: Int ! P = factor.flatMap(f => many[Int, P](char[P]('*').flatMap(_ => factor)).map(fs => (f :: fs).product))
  def expr: Int ! P = term.flatMap(t => many[Int, P](char[P]('+').flatMap(_ => term)).map(ts => (t :: ts).sum))

  // ---- JSON: the grammar of docs/modules/parsing.md, in the shared syntax, on both implementations
  sealed trait Json
  case object JNull extends Json
  final case class JBool(b: Boolean) extends Json
  final case class JNum(n: BigDecimal) extends Json
  final case class JStr(s: String) extends Json
  final case class JArr(items: List[Json]) extends Json
  final case class JObj(fields: List[(String, Json)]) extends Json

  def ws: Unit ! P = many[Char, P](satisfy[P](_.isWhitespace)).as(())
  def token(c: Char): Unit ! P = ws.flatMap(_ => char[P](c)).flatMap(_ => ws)
  def sepBy[A](p: A ! P, sep: Char): List[A] ! P =
    orElse[List[A], P](p.flatMap(a => many[A, P](token(sep).flatMap(_ => p)).map(a :: _)), pure[List[A], P](Nil))
  def digits: String ! P = many1[Char, P](satisfy[P](_.isDigit)).map(_.mkString)
  def optional(p: String ! P): String ! P = orElse[String, P](p, pure[String, P](""))
  def jnumber: Json ! P =
    optional(char[P]('-').as("-")).flatMap(sign => digits.flatMap(int => optional(char[P]('.').flatMap(_ => digits).map("." + _)).flatMap(frac =>
      optional(satisfy[P](c => c == 'e' || c == 'E').flatMap(_ => optional(satisfy[P](c => c == '+' || c == '-').map(_.toString))).flatMap(s => digits.map("e" + s + _)))
        .map(exp => JNum(BigDecimal(sign + int + frac + exp))))))
  def hex: Char ! P = satisfy[P](c => c.isDigit || ('a' to 'f').contains(c.toLower))
  def escape: Char ! P = char[P]('\\').flatMap(_ => item[P]).flatMap {
    case 'n' => pure[Char, P]('\n')
    case 't' => pure[Char, P]('\t')
    case 'r' => pure[Char, P]('\r')
    case 'b' => pure[Char, P]('\b')
    case 'f' => pure[Char, P]('\f')
    case 'u' => hex.flatMap(a => hex.flatMap(b => hex.flatMap(c => hex.map(d => Integer.parseInt(s"$a$b$c$d", 16).toChar))))
    case c => pure[Char, P](c)
  }
  def text: String ! P =
    char[P]('"').flatMap(_ => many[Char, P](orElse[Char, P](satisfy[P](c => c != '"' && c != '\\'), escape))).flatMap(cs => char[P]('"').as(cs.mkString))
  def literal(word: String, j: Json): Json ! P = string[P](word).as(j)
  def value: Json ! P = ws.flatMap(_ => orElse[Json, P](literal("null", JNull), orElse[Json, P](literal("true", JBool(true)),
    orElse[Json, P](literal("false", JBool(false)), orElse[Json, P](jnumber, orElse[Json, P](text.map(JStr(_)),
    orElse[Json, P](array, obj))))))).flatMap(j => ws.as(j))
  def array: Json ! P = char[P]('[').flatMap(_ => sepBy(value, ',')).flatMap(items => token(']').as(JArr(items)))
  def obj: Json ! P =
    char[P]('{').flatMap(_ => sepBy(ws.flatMap(_ => text).flatMap(k => token(':').flatMap(_ => value).map(k -> _)), ',')).flatMap(fs => token('}').as(JObj(fs)))

  test("JSON: a grammar on the module") {
    assertEquals(ps.fx.run(parse("""{"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"y\u00e9"}, "e": []}""")(value)),
      Some(JObj(List("a" -> JArr(List(JNum(1), JNum(-250), JBool(true))), "b" -> JObj(List("c" -> JNull, "d" -> JStr("x\"yé"))), "e" -> JArr(Nil)))))
    assertEquals(ps.fx.run(parse(" [ ] ")(value)), Some(JArr(Nil)))
    assertEquals(ps.fx.run(parse("[1, 2")(value)), None)
    assertEquals(ps.fx.run(parse("""{"a": }""")(value)), None)
  }

  test("an arithmetic expression") {
    assertEquals(ps.fx.run(parse("2+3*(4+1)")(expr)), Some(17))
    assertEquals(ps.fx.run(parse("12*12")(expr)), Some(144))
  }
  test("all of the input, or nothing") {
    assertEquals(ps.fx.run(parse("2+")(expr)), None)
    assertEquals(ps.fx.run(parse("(1")(expr)), None)
  }
  test("a word among others: backtracking") {
    assertEquals(ps.fx.run(parse("letter")(orElse[String, P](string[P]("let"), string[P]("letter")))), Some("letter"))
  }
}

class ParsingOnMachine extends ParsingChecks(Machine)
class ParsingOnFree extends ParsingChecks(Free)
class ParsingOnTagless extends ParsingChecks(okay.effects.tagless.Tagless.on(Machine))
