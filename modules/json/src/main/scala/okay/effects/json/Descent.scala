package okay.effects.json

import okay.effects.parsing.ParseError

/** JSON BY RECURSIVE DESCENT: a hand-written parser, an index over the text, the JVM's stack — the fast handler of
 * `JsonParser`, 19× the grammar on effects (bench.d). Strict as RFC 8259, the same errors as the grammar's: the
 * furthest point and what was expected there, so a test can hold the two to the same answers. `maxDepth` bounds the
 * nesting, and so the stack */
object Descent {
  def parse(text: String, maxDepth: Int = 1000): Either[ParseError, Json] = {
    val p = new Parser(text, maxDepth)
    try {
      p.ws(); val v = p.value(0); p.ws()
      if (p.i < text.length) p.fail("end of input") else Right(v)
    } catch { case Failed(at, what) => Left(ParseError.at(text, at, List(what))) }
  }

  private final case class Failed(at: Int, what: String) extends RuntimeException(what, null, false, false)

  private final class Parser(s: String, maxDepth: Int) {
    var i = 0
    def fail(what: String): Nothing = throw Failed(i, what)
    def isSpace(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
    def isHex(c: Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    def ws(): Unit = while (i < s.length && isSpace(s.charAt(i))) i += 1
    def peek: Int = if (i < s.length) s.charAt(i) else -1
    def word(w: String, v: Json): Json = if (s.startsWith(w, i)) { i += w.length; v } else fail(s"'$w'")
    def value(depth: Int): Json = {
      ws()
      val v: Json = peek match {
        case '{' => obj(depth)
        case '[' => arr(depth)
        case '"' => Json.Str(text())
        case 't' => word("true", Json.Bool(true))
        case 'f' => word("false", Json.Bool(false))
        case 'n' => word("null", Json.Null)
        case c if c == '-' || (c >= '0' && c <= '9') => number()
        case _ => fail("a JSON value")
      }
      ws(); v
    }
    def deeper(depth: Int): Unit = if (depth >= maxDepth) fail(s"nesting of at most $maxDepth")
    def arr(depth: Int): Json = {
      deeper(depth); i += 1; ws()
      if (peek == ']') { i += 1; return Json.Arr(Vector.empty) }
      val items = Vector.newBuilder[Json]
      while (true) {
        items += value(depth + 1)
        peek match {
          case ',' => i += 1
          case ']' => i += 1; return Json.Arr(items.result())
          case _ => fail("',' or ']'")
        }
      }
      Json.Arr(items.result())
    }
    def obj(depth: Int): Json = {
      deeper(depth); i += 1; ws()
      if (peek == '}') { i += 1; return Json.Obj.empty }
      val fields = Vector.newBuilder[(String, Json)]
      while (true) {
        ws(); if (peek != '"') fail("a field name in quotes")
        val k = text(); ws()
        if (peek != ':') fail("':'"); i += 1
        val v = value(depth + 1); fields += (k -> v)
        peek match {
          case ',' => i += 1
          case '}' => i += 1; return Json.Obj(fields.result())
          case _ => fail("',' or '}'")
        }
      }
      Json.Obj(fields.result())
    }
    /** a string: the quote is next */
    def text(): String = {
      i += 1; val b = new java.lang.StringBuilder
      while (true) {
        val start = i
        while (i < s.length && { val c = s.charAt(i); c != '"' && c != '\\' && c >= ' ' }) i += 1
        b.append(s, start, i)
        peek match {
          case '"' => i += 1; return b.toString
          case '\\' => i += 1; escape(b)
          case c if c >= 0 && c < ' ' => fail("no control character in a string")
          case _ => fail("the closing '\"'")
        }
      }
      b.toString
    }
    def escape(b: java.lang.StringBuilder): Unit = peek match {
      case '"' => i += 1; b.append('"')
      case '\\' => i += 1; b.append('\\')
      case '/' => i += 1; b.append('/')
      case 'b' => i += 1; b.append('\b')
      case 'f' => i += 1; b.append('\f')
      case 'n' => i += 1; b.append('\n')
      case 'r' => i += 1; b.append('\r')
      case 't' => i += 1; b.append('\t')
      case 'u' =>
        i += 1; var code = 0; var n = 0
        while (n < 4) { val c = peek; if (c < 0 || !isHex(c.toChar)) fail("a hex digit"); code = code * 16 + Character.digit(c.toChar, 16); i += 1; n += 1 }
        b.append(code.toChar)
      case _ => fail("an escape: \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\uXXXX")
    }
    def digits(what: String): Unit = { val start = i; while (i < s.length && isDigit(s.charAt(i))) i += 1; if (i == start) fail(what) }
    def number(): Json = {
      val start = i
      if (peek == '-') i += 1
      if (peek == '0') { i += 1; if (peek >= 0 && isDigit(peek.toChar)) fail("no leading zero") } else digits("a digit")
      if (peek == '.') { i += 1; digits("a digit after the point") }
      if (peek == 'e' || peek == 'E') {
        i += 1; if (peek == '+' || peek == '-') i += 1
        digits("a digit of the exponent")
      }
      Json.Num(BigDecimal(s.substring(start, i)))
    }
  }
}
