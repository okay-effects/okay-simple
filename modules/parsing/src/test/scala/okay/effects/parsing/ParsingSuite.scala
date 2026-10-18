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
