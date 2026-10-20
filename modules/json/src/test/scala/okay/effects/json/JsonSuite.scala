package okay.effects.json.checks

import okay.effects.json.{Json, Jsons}
import okay.effects.json.Json._
import okay.effects.parsing.ParseError
import okay.effects.stream.Streams
import okay.effects.{Runner, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class JsonChecks(val fx0: Runner) extends munit.FunSuite {
  implicit val runner: Runner = fx0
  val streams = new Streams[fx0.type]()(fx0)
  val js = new Jsons[fx0.type, streams.type](streams)

  def parse(s: String, maxDepth: Int = 1000): Either[ParseError, Json] = fx0.run(js.parser(s, maxDepth))
  def failure(s: String): (Long, List[String], Option[String]) = parse(s).left.map(e => (e.offset, e.expected, e.found)).left.getOrElse(fail(s"parsed: $s"))

  val sample: Json = obj("name" -> Str("Ann \"the\" Fast\n"), "age" -> num(41), "tags" -> arr(Str("a"), Str("b")),
    "price" -> Num(BigDecimal("12.50")), "none" -> Null, "ok" -> Bool(true), "nested" -> obj("deep" -> arr()))

  test("print and parse round trip") {
    assertEquals(parse(Json.print(sample)), Right(sample))
    assertEquals(parse(Json.pretty(sample)), Right(sample))
  }
  test("the values") {
    assertEquals(parse(""" {"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"y\u00e9\n\/"}, "e": [], "f": {}} """),
      Right(obj("a" -> arr(num(1), Num(BigDecimal(-250)), Bool(true)), "b" -> obj("c" -> Null, "d" -> Str("x\"yé\n/")), "e" -> arr(), "f" -> Obj.empty)))
    assertEquals(parse("-0.5"), Right(Num(BigDecimal("-0.5"))))
    assertEquals(parse("1E+3"), Right(Num(BigDecimal("1E+3"))))
    assertEquals(parse("0"), Right(num(0)))
    assertEquals(parse("\"\""), Right(Str("")))
  }
  test("errors say where and what was expected") {
    assertEquals(failure("[1, 2"), (5L, List("',' or ']'"), Some("end of input")))
    assertEquals(failure("""{"a": }"""), (6L, List("a JSON value"), Some("'}'")))
    assertEquals(failure("tru"), (0L, List("'true'"), Some("'t'")))
    assertEquals(failure("""{"a":1,}"""), (7L, List("a field name in quotes"), Some("'}'")))
    assertEquals(failure("\"abc"), (4L, List("the closing '\"'"), Some("end of input")))
    assertEquals(failure("[1,,2]"), (3L, List("a JSON value"), Some("','")))
    assertEquals(failure("01"), (1L, List("no leading zero"), Some("'1'")))
    assertEquals(failure("-"), (1L, List("a digit"), Some("end of input")))
    assertEquals(parse("01").left.map(_.message), Left("at line 1, column 2: expected no leading zero, found '1'"))
    assertEquals(failure("{} x"), (3L, List("end of input"), Some("'x'")))
    assertEquals(failure("\"a\tb\""), (2L, List("no control character in a string"), Some("'\t'")))
    assertEquals(failure("\"\\x\""), (2L, List("an escape: \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\uXXXX"), Some("'x'")))
    assertEquals(failure("1."), (2L, List("a digit after the point"), Some("end of input")))
    assertEquals(failure(""), (0L, List("a JSON value"), Some("end of input")))
  }
  test("nesting: deep on the heap, bounded by choice") {
    val n = 100000
    assertEquals(parse("[" * n + "]" * n, maxDepth = n + 1).map(_ => "parsed"), Right("parsed"))
    assertEquals(parse("[" * n + "]" * n).left.map(e => (e.offset, e.expected)), Left((1000L, List("nesting of at most 1000"))))
    assertEquals(parse("[" * 10 + "]" * 10, maxDepth = 5).left.map(e => (e.offset, e.expected)), Left((5L, List("nesting of at most 5"))))
  }
  test("values from a stream of chunks, as they come; a failure the last element") {
    val text = """{"a": 1} {"b": [1, 2]}
      |"line two"
      |42 [""".stripMargin
    val chunks = streams.fromIterator[String, Void](text.grouped(7))
    val got = fx0.run(js.values[Void](chunks).toList)
    assertEquals(got.take(4), List(Right(obj("a" -> num(1))), Right(obj("b" -> arr(num(1), num(2)))), Right(Str("line two")), Right(num(42))))
    assertEquals(got.drop(4).map(_.left.map(e => (e.offset, e.expected))), List(Left((text.length.toLong, List("a JSON value")))))
    assertEquals(fx0.run(js.values[Void](streams.fromIterator[String, Void](Iterator("  ", " "))).toList), Nil)
  }
  test("a stream pulled only as far as asked") {
    var pulled = 0
    val chunks = streams.fromIterator[String, Void](Iterator.continually { pulled += 1; "1 " })
    assertEquals(fx0.run(js.values[Void](chunks).take(3).toList), List(Right(num(1)), Right(num(1)), Right(num(1))))
    assert(pulled <= 5, s"pulled $pulled chunks for three values")
  }
}

class JsonOnMachine extends JsonChecks(Machine)
class JsonOnFree extends JsonChecks(Free)
