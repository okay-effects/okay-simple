package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState}
import okay.effects.Runner
import okay.effects.json.{Json, Jsons}
import okay.effects.machine.Machine
import okay.effects.stream.Streams

/** THE JSON PARSER ON EFFECTS against a hand-written recursive descent (`HandJson`, the same `Json`), on the machine
 * and on `Free`, texts of 5 KB and 100 KB: `sbt "bench/Jmh/run -f 1 -wi 3 -i 5 JsonBenchmark"`. The ratio is the cost
 * of parsing by effects; the history is in bench.d/ */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsonBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  @Param(Array("5k", "100k"))
  var size: String = ""
  var text: String = ""
  var parse: String => Either[Any, Json] = null

  @Setup def setup(): Unit = {
    val record = """{"name":"Ann","age":41,"tags":["a","b"],"born":"1985-03-09","email":null,"score":12.5,"ok":true}"""
    val n = if (size == "5k") 50 else 1000
    text = "[" + List.fill(n)(record).mkString(",") + "]"
    val fx: Runner = if (impl == "machine") Machine else okay.effects.free.Free
    val streams = new Streams[fx.type]()(fx)
    val js = new Jsons[fx.type, streams.type](streams)
    parse = s => fx.run(js.parser(s))
  }

  @Benchmark def effects: Boolean = parse(text).isRight
  @Benchmark def hand: Boolean = HandJson.parse(text).isRight
}

/** a hand-written recursive descent to the same `Json`: the reference the parser on effects is measured against */
object HandJson {
  def parse(s: String): Either[String, Json] = {
    val p = new Parser(s)
    try { p.ws(); val v = p.value(); p.ws(); if (p.i < s.length) Left("text after the value") else Right(v) }
    catch { case e: IllegalArgumentException => Left(e.getMessage) }
  }
  private final class Parser(s: String) {
    var i = 0
    def fail(msg: String): Nothing = throw new IllegalArgumentException(s"at $i: $msg")
    def ws(): Unit = while (i < s.length && (s.charAt(i) == ' ' || s.charAt(i) == '\n' || s.charAt(i) == '\r' || s.charAt(i) == '\t')) i += 1
    def peek: Char = if (i < s.length) s.charAt(i) else fail("the text ends too soon")
    def expect(c: Char): Unit = if (peek == c) i += 1 else fail(s"expected '$c'")
    def word(w: String, v: Json): Json = if (s.startsWith(w, i)) { i += w.length; v } else fail(s"expected $w")
    def value(): Json = peek match {
      case '{' => obj()
      case '[' => arr()
      case '"' => Json.Str(string())
      case 't' => word("true", Json.Bool(true))
      case 'f' => word("false", Json.Bool(false))
      case 'n' => word("null", Json.Null)
      case c if c == '-' || (c >= '0' && c <= '9') => number()
      case c => fail(s"unexpected '$c'")
    }
    def obj(): Json = {
      expect('{'); ws(); val fields = Vector.newBuilder[(String, Json)]
      if (peek == '}') { i += 1; return Json.Obj(fields.result()) }
      while (true) {
        ws(); if (peek != '"') fail("expected a field name")
        val k = string(); ws(); expect(':'); ws(); val v = value(); fields += (k -> v); ws()
        peek match { case ',' => i += 1; case '}' => i += 1; return Json.Obj(fields.result()); case _ => fail("expected ',' or '}'") }
      }
      Json.Obj(fields.result())
    }
    def arr(): Json = {
      expect('['); ws(); val items = Vector.newBuilder[Json]
      if (peek == ']') { i += 1; return Json.Arr(items.result()) }
      while (true) {
        ws(); items += value(); ws()
        peek match { case ',' => i += 1; case ']' => i += 1; return Json.Arr(items.result()); case _ => fail("expected ',' or ']'") }
      }
      Json.Arr(items.result())
    }
    def string(): String = {
      expect('"'); val b = new StringBuilder
      while (true) {
        val c = peek; i += 1
        c match {
          case '"' => return b.toString
          case '\\' =>
            val e = peek; i += 1
            e match {
              case '"' => b.append('"'); case '\\' => b.append('\\'); case '/' => b.append('/')
              case 'n' => b.append('\n'); case 'r' => b.append('\r'); case 't' => b.append('\t')
              case 'b' => b.append('\b'); case 'f' => b.append('\f')
              case 'u' => b.append(Integer.parseInt(s.substring(i, i + 4), 16).toChar); i += 4
              case other => fail(s"bad escape '\\$other'")
            }
          case c if c < 0x20 => fail("a control character in a string")
          case c => b.append(c)
        }
      }
      b.toString
    }
    def number(): Json = {
      val start = i
      if (peek == '-') i += 1
      while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      if (i < s.length && s.charAt(i) == '.') { i += 1; while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1 }
      if (i < s.length && (s.charAt(i) == 'e' || s.charAt(i) == 'E')) {
        i += 1; if (i < s.length && (s.charAt(i) == '+' || s.charAt(i) == '-')) i += 1
        while (i < s.length && s.charAt(i) >= '0' && s.charAt(i) <= '9') i += 1
      }
      Json.Num(BigDecimal(s.substring(start, i)))
    }
  }
}
