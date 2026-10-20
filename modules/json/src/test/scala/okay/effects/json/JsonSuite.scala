package okay.effects.json.checks

import okay.effects.json.{Descent, Format, Json, JsonParser, JsonWriter, Jsons}
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
  /** the grammar and the descent must agree, value and error alike */
  def agree(s: String, maxDepth: Int = 1000): Unit = assertEquals(Descent.parse(s, maxDepth), parse(s, maxDepth), s"descent vs grammar on: $s")
  def failure(s: String): (Long, List[String], Option[String]) = { agree(s); parse(s).left.map(e => (e.offset, e.expected, e.found)).left.getOrElse(fail(s"parsed: $s")) }

  val sample: Json = obj("name" -> Str("Ann \"the\" Fast\n"), "age" -> num(41), "tags" -> arr(Str("a"), Str("b")),
    "price" -> Num(BigDecimal("12.50")), "none" -> Null, "ok" -> Bool(true), "nested" -> obj("deep" -> arr()))

  test("print and parse round trip") {
    assertEquals(parse(Json.print(sample)), Right(sample))
    assertEquals(parse(Json.pretty(sample)), Right(sample))
    agree(Json.print(sample)); agree(Json.pretty(sample))
  }
  test("the effect: one program, the handler chosen outside") {
    type R[X] = okay.effects.Sum[Void, JsonParser, X]
    import js.streams.fx._
    val program: Either[ParseError, Json] ! R = js.parseJson[R]("""{"a": [1, 2]}""")
    assertEquals(fx0.run(js.descent(program)), Right(obj("a" -> arr(num(1), num(2)))))
    assertEquals(fx0.run(js.grammar(program)), Right(obj("a" -> arr(num(1), num(2)))))
    val bad: Either[ParseError, Json] ! R = js.parseJson[R]("[1, 2")
    assertEquals(fx0.run(js.descent(bad)), fx0.run(js.grammar(bad)))
  }
  test("the values") {
    assertEquals(parse(""" {"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"y\u00e9\n\/"}, "e": [], "f": {}} """),
      Right(obj("a" -> arr(num(1), Num(BigDecimal(-250)), Bool(true)), "b" -> obj("c" -> Null, "d" -> Str("x\"yé\n/")), "e" -> arr(), "f" -> Obj.empty)))
    List(""" {"a": [1, -2.5e2, true], "b": {"c": null, "d": "x\"y\u00e9\n\/"}, "e": [], "f": {}} """, "-0.5", "1E+3", "0", "\"\"", "[[[]]]", " [ ] ").foreach(agree(_))
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
    agree("[" * 10 + "]" * 10, maxDepth = 5); agree("[" * 500 + "]" * 500)
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
  test("the way out: compact, pretty, ascii; the printer an effect with the format its handler's") {
    val j = obj("a" -> arr(num(1), Str("é\"x")), "b" -> Obj.empty, "c" -> arr())
    assertEquals(Json.print(j), """{"a":[1,"é\"x"],"b":{},"c":[]}""")
    assertEquals(Json.print(j, ascii = true), "{\"a\":[1,\"\\u00e9\\\"x\"],\"b\":{},\"c\":[]}")   // not a triple-quoted literal: Scala 2 would read the \u in it
    assertEquals(Json.pretty(j), "{\n  \"a\": [\n    1,\n    \"é\\\"x\"\n  ],\n  \"b\": {},\n  \"c\": []\n}")
    // the writer: WHERE is the handler's — a string, a sink of the row, a stream
    type R[X] = okay.effects.Sum[Void, JsonWriter, X]
    import js.streams.fx._
    import js.streams.fx.std._
    val program: Int ! R = js.writeJson[R](j).flatMap(_ => js.writeJson[R](num(7))).map(_ => 42)
    assertEquals(fx0.run(js.collected()(program)), (Json.print(j) + "\n7", 42))
    assertEquals(fx0.run(js.collected(Format.Indented(4), separator = "\n---\n")(program)), (Json.pretty(j, 4) + "\n---\n7", 42))
    assertEquals(fx0.run(js.collected(Format.Compact(ascii = true))(program))._1, Json.print(j, ascii = true) + "\n7")
    // to a sink of the row outside: here a Writer's `tell`; a console's line or a socket's send are the same shape
    type W[X] = okay.effects.Sum[Void, ({ type L[Z] = Writer[String, Z] })#L, X]
    type RW[X] = okay.effects.Sum[W, JsonWriter, X]
    val told: Int ! RW = js.writeJson[RW](j).flatMap(_ => js.writeJson[RW](num(7))).map(_ => 42)
    assertEquals(fx0.run(writer[String](js.writing[W]((s: String) => tell[String, W](s))(told))), (List(Json.print(j), "7"), 42))
    // as a stream, pulled as the program writes
    assertEquals(fx0.run(js.streamed[Void]()(program).toList), List(Json.print(j), "7"))
  }
  test("printing is iterative: a hundred thousand deep, both ways") {
    val n = 100000
    val deep = parse("[" * n + "]" * n, maxDepth = n + 1).toOption.get
    val text = Json.print(deep)
    assertEquals(text.length, 2 * n)
    // not `==` on the values: a case class's equals recurses on the stack; the text is the comparison
    assertEquals(parse(text, maxDepth = n + 1).map(Json.print(_)), Right(text))
    // `pretty` of it is not asked: its indentation alone is quadratic in the depth
  }
  test("chunks: a value as the stream of its pieces; lines: a value a line") {
    val j = obj("a" -> arr(num(1), num(2)), "b" -> Str("x"), "c" -> Null)
    val pieces = fx0.run(js.chunks[Void](j).toList)
    assertEquals(pieces.mkString, Json.print(j))
    assert(pieces.size > 5)
    val deep = parse("[" * 100000 + "]" * 100000, maxDepth = 100001).toOption.get
    assertEquals(fx0.run(js.chunks[Void](deep).fold(0)((n, s) => n + s.length)), 200000)
    val lines = fx0.run(js.toLines[Void](streams.emits[Json, Void](List(num(1), obj("a" -> Null)))).toList)
    assertEquals(lines, List("1\n", """{"a":null}""" + "\n"))
    assertEquals(fx0.run(js.values[Void](streams.emits[String, Void](lines)).toList), List(Right(num(1)), Right(obj("a" -> Null))))
    // lazy: the first piece of a deep value costs no walk of the rest
    assertEquals(fx0.run(js.chunks[Void](deep).take(2).toList), List("[", "["))
  }
  test("streams: a chunk boundary anywhere — inside a string, an escape, a literal, a number — changes nothing") {
    val text = """{"a": [1, -2.5e+2, true, null], "b": "x\"y\u00e9\n", "c": {}} 42 "z" [] false"""
    val whole = fx0.run(js.values[Void](streams.emits[String, Void](List(text))).toList)
    assertEquals(whole.size, 5)
    assert(whole.forall(_.isRight))
    (1 until text.length).foreach { cut =>
      val two = List(text.substring(0, cut), text.substring(cut))
      assertEquals(fx0.run(js.values[Void](streams.emits[String, Void](two)).toList), whole, s"cut at $cut")
    }
    val many = text.grouped(3).toList
    assertEquals(fx0.run(js.values[Void](streams.emits[String, Void](many)).toList), whole)
    assertEquals(fx0.run(js.values[Void](streams.emits[String, Void](text.map(_.toString).toList)).toList), whole)
  }
  test("JSON lines: lines cut across chunks, \\r\\n, blank lines, a bad line reported by number and the rest read") {
    val text = "{\"a\": 1}\r\n\n[1, 2\n  \n\"ok\"\n3"
    val expected = List(Right(obj("a" -> num(1))), Left(3L), Right(Str("ok")), Right(num(3)))
    def read(chunks: List[String]) = fx0.run(js.fromLines[Void](streams.emits[String, Void](chunks)).toList).map(_.left.map(_.line))
    assertEquals(read(List(text)), expected)
    (1 until text.length).foreach(cut => assertEquals(read(List(text.substring(0, cut), text.substring(cut))), expected, s"cut at $cut"))
    assertEquals(read(text.map(_.toString).toList), expected)
    assertEquals(fx0.run(js.fromLines[Void](streams.emits[String, Void](List("[1, 2\n"))).toList).map(_.left.map(e => (e.line, e.error.offset, e.error.expected))), List(Left((1L, 5L, List("',' or ']'")))))
    assertEquals(fx0.run(js.splitLines[Void](streams.emits[String, Void](List("a\nb", "c\r\n", "\nd"))).toList), List("a", "bc", "", "d"))
    assertEquals(fx0.run(js.splitLines[Void](streams.emits[String, Void](Nil)).toList), Nil)
  }
  test("JSON lines: two hundred thousand lines through, as far as asked, in constant memory") {
    val n = 200000
    var produced = 0
    val lines = streams.fromIterator[String, Void](Iterator.tabulate(n) { i => produced += 1; s"""{"i": $i, "tags": ["a", "b"]}""" + "\n" })
    val count = fx0.run(js.fromLines[Void](lines).fold(0L)((c, e) => if (e.isRight) c + 1 else c))
    assertEquals(count, n.toLong)
    produced = 0
    val lazyLines = streams.fromIterator[String, Void](Iterator.tabulate(n) { i => produced += 1; s"$i\n" })
    assertEquals(fx0.run(js.fromLines[Void](lazyLines).take(3).toList), List(Right(num(0)), Right(num(1)), Right(num(2))))
    assert(produced <= 5, s"produced $produced chunks for three lines")
  }
}

class JsonOnMachine extends JsonChecks(Machine)
class JsonOnFree extends JsonChecks(Free)
