package okay.effects.parsing.checks

import okay.effects.parsing.{Expect, Input, ParseError, Parsing, StreamInput}
import okay.effects.stream.Streams
import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ParsingChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val ps = Parsing()
  import ps._
  import ps.fx._
  val streams = new Streams[ps.fx.type]()(ps.fx)
  val fromStreams = new StreamInput[streams.type](streams)

  type P[X] = Sum[({ type L[x] = Sum[({ type M[y] = Sum[Void, Std.Choose, y] })#M, Expect, x] })#L, Input, X]

  def number: Int ! P = takeWhile1[P](_.isDigit, "a digit").map(_.toInt)
  def factor: Int ! P = orElse[Int, P](number, char[P]('(').flatMap(_ => expr).flatMap(e => char[P](')').as(e)))
  def term: Int ! P = factor.flatMap(f => many[Int, P](char[P]('*').flatMap(_ => factor)).map(fs => (f :: fs).product))
  def expr: Int ! P = term.flatMap(t => many[Int, P](char[P]('+').flatMap(_ => term)).map(ts => (t :: ts).sum))

  def run[A](p: Either[ParseError, A] ! Void): Either[ParseError, A] = ps.fx.run(p)

  test("an arithmetic expression") {
    assertEquals(run(parse("2+3*(4+1)")(expr)), Right(17))
    assertEquals(run(parse("12*12")(expr)), Right(144))
  }
  test("all of the input, or an error at the furthest point, saying what was expected there") {
    assertEquals(run(parse("2+")(expr)).left.map(e => (e.offset, e.expected, e.found)), Left((2L, List("a digit", "'('"), Some("end of input"))))
    assertEquals(run(parse("(1")(expr)).left.map(e => (e.offset, e.expected)), Left((2L, List("'*'", "'+'", "')'"))))
    assertEquals(run(parse("2+3)")(expr)).left.map(_.message), Left("at line 1, column 4: expected '*' or '+' or end of input, found ')'"))
  }
  test("a word among others: backtracking") {
    assertEquals(run(parse("letter")(orElse[String, P](string[P]("let"), string[P]("letter")))), Right("letter"))
    assertEquals(run(parse("let")(orElse[String, P](string[P]("letter"), string[P]("let")))), Right("let"))
  }
  test("lines and columns") {
    assertEquals(ParseError.at("ab\ncd\nef", 7, List("x")), ParseError(7, 3, 2, List("x"), Some("'f'")))
    assertEquals(ParseError.at("ab\ncd", 5, Nil), ParseError(5, 2, 3, Nil, Some("end of input")))
    assertEquals(ParseError.at("ab", 9, Nil).message, "at line 1, column 3: no parse, found end of input")
  }
  test("decided by the next character: no choice point") {
    def word: String ! P = byNext[String, P]("a word") {
      case 'a' => Some(string[P]("apple"))
      case 'b' => Some(string[P]("banana"))
      case _ => None
    }
    def words: List[String] ! P = whileNext[String, P](c => c == 'a' || c == 'b')(word.flatMap(w => skipWhile[P](_ == ' ').as(w)))
    assertEquals(run(parse("apple banana apple")(words)), Right(List("apple", "banana", "apple")))
    assertEquals(run(parse("apple cherry")(words)).left.map(e => (e.offset, e.expected)), Left((6L, List("end of input"))))
    assertEquals(run(parse("apricot")(words)).left.map(e => (e.offset, e.expected, e.found)), Left((0L, List("'apple'"), Some("'a'"))))
  }
  test("deep nesting is the heap's, not the stack's") {
    val n = 100000
    assertEquals(run(parse("(" * n + "1" + ")" * n)(expr)), Right(1))
  }
  test("the input from a stream of chunks, pulled as needed") {
    val chunks = streams.fromIterator[String, Void](Iterator("2+3*", "(4", "+1)"))
    assertEquals(ps.fx.run(fromStreams.parseStream[Void](chunks)(expr)), Right(17))
    val cut = streams.fromIterator[String, Void](Iterator("2+3*", "(4"))
    assertEquals(ps.fx.run(fromStreams.parseStream[Void](cut)(expr)).left.map(e => (e.offset, e.expected)), Left((6L, List("'*'", "'+'", "')'"))))
    assertEquals(ps.fx.run(fromStreams.parseStream[Void](streams.fromIterator[String, Void](Iterator("let", "ter")))(
      orElse[String, P](string[P]("let"), string[P]("letter")))), Right("letter"))
  }
}

class ParsingOnMachine extends ParsingChecks(Machine)
class ParsingOnFree extends ParsingChecks(Free)
