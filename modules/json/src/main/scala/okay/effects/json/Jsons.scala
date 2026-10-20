package okay.effects.json

import okay.effects.parsing.{Expect, Input, ParseError, Parsing, StreamInput}
import okay.effects.stream.Streams

/** THE EFFECT: a text to a `Json`. How is the handler's choice — `descent`, a hand-written recursive descent, fast, the
 * default; `grammar`, the grammar on the parsing module's effects, on the heap, the one `values` streams with. A program
 * that parses says only that it parses */
sealed trait JsonParser[X]
object JsonParser {
  final case class Parse[X](text: String, maxDepth: Int, answer: Either[ParseError, Json] => X) extends JsonParser[X]
}

/** THE EFFECT THE OTHER WAY: a `Json` WRITTEN — and where to, is the handler's: into a string (`collected`), to any
 * sink the row has, a console or a socket (`writing`), as a stream of pieces (`streamed`); and how, compact or
 * indented, ASCII-only or not, is the `Format` the handler is given */
sealed trait JsonWriter[X]
object JsonWriter {
  final case class Write[X](json: Json, answer: Unit => X) extends JsonWriter[X]
}

/** how a value is written: one line for a wire, indented for a person; `ascii` escapes every character beyond ASCII */
sealed trait Format { def render(j: Json): String }
object Format {
  final case class Compact(ascii: Boolean = false) extends Format { def render(j: Json): String = Json.print(j, ascii) }
  final case class Indented(indent: Int = 2, ascii: Boolean = false) extends Format { def render(j: Json): String = Json.pretty(j, indent, ascii) }
}

/**
 * JSON: the effect `JsonParser` and its two handlers, and the grammar itself. THE GRAMMAR is a program of the parsing
 * module's three effects, decided by the next character —
 * no choice point, no backtracking, one operation per token — so it runs as a straight line of operations answered in
 * place, on the heap: a text nested a hundred thousand deep is parsed, not a stack overflow (`maxDepth` says where to
 * stop, by choice). From a text in memory (`parse`), or from a stream of chunks as values come (`values`), with an
 * error saying where and what was expected. Over the streams given, so its streams are the caller's.
 */
class Jsons[I <: okay.effects.Effects, S <: Streams[I]](val streams: S) {
  import streams.fx._
  import streams.fx.std.{Choose, Yield, emit, first}
  val parsers: Parsing[streams.fx.type] = new Parsing[streams.fx.type]()(streams.fx)
  import parsers._
  private val fromStreams: StreamInput[streams.type] = new StreamInput[streams.type](streams)

  /** JSON's whitespace: a space, a tab, a line feed, a carriage return */
  def isSpace(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'
  private def isDigit(c: Char): Boolean = c >= '0' && c <= '9'
  private def isHex(c: Char): Boolean = isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  /** a JSON value, surrounding whitespace taken; nested at most `maxDepth` */
  def value[R[_]](maxDepth: Int = 1000)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Json ! R = {
    def ws: Unit ! R = skipWhile[R](isSpace)
    def at(depth: Int): Json ! R = ws.flatMap(_ => byNext[Json, R]("a JSON value") {
      case '{' => Some(obj(depth))
      case '[' => Some(arr(depth))
      case '"' => Some(text.map(Json.Str(_)))
      case 't' => Some(string[R]("true").as(Json.Bool(true): Json))
      case 'f' => Some(string[R]("false").as(Json.Bool(false): Json))
      case 'n' => Some(string[R]("null").as(Json.Null: Json))
      case ch if ch == '-' || isDigit(ch) => Some(number)
      case _ => None
    }).flatMap(j => ws.as(j))
    def deeper(depth: Int): Unit ! R =
      if (depth >= maxDepth) fail[Unit, R](s"nesting of at most $maxDepth") else pure[Unit, R](())
    def arr(depth: Int): Json ! R = deeper(depth).flatMap(_ => take[R]).flatMap(_ => ws).flatMap(_ => peek[R]).flatMap {
      case Some(']') => take[R].as(Json.Arr(Vector.empty): Json)
      case _ => elements(depth, Vector.newBuilder[Json])
    }
    def elements(depth: Int, acc: scala.collection.mutable.Builder[Json, Vector[Json]]): Json ! R =
      at(depth + 1).flatMap { j => acc += j; byNext[Json, R]("',' or ']'") {
        case ',' => Some(take[R].flatMap(_ => elements(depth, acc)))
        case ']' => Some(take[R].as(Json.Arr(acc.result()): Json))
        case _ => None
      } }
    def obj(depth: Int): Json ! R = deeper(depth).flatMap(_ => take[R]).flatMap(_ => ws).flatMap(_ => peek[R]).flatMap {
      case Some('}') => take[R].as(Json.Obj.empty: Json)
      case _ => members(depth, Vector.newBuilder[(String, Json)])
    }
    def members(depth: Int, acc: scala.collection.mutable.Builder[(String, Json), Vector[(String, Json)]]): Json ! R =
      ws.flatMap(_ => byNext[String, R]("a field name in quotes") { case '"' => Some(text); case _ => None })
        .flatMap(k => ws.flatMap(_ => char[R](':')).flatMap(_ => at(depth + 1)).flatMap { v => acc += (k -> v); byNext[Json, R]("',' or '}'") {
          case ',' => Some(take[R].flatMap(_ => members(depth, acc)))
          case '}' => Some(take[R].as(Json.Obj(acc.result()): Json))
          case _ => None
        } })
    /** a string: the quote is next */
    def text: String ! R = take[R].flatMap(_ => chars(new StringBuilder))
    def chars(sb: StringBuilder): String ! R =
      takeWhile[R](ch => ch != '"' && ch != '\\' && ch >= ' ').flatMap { s => sb.append(s); byNext[String, R]("the closing '\"'") {
        case '"' => Some(take[R].as(sb.toString))
        case '\\' => Some(take[R].flatMap(_ => escape(sb)).flatMap(_ => chars(sb)))
        case ch if ch < ' ' => Some(fail[String, R]("no control character in a string"))
        case _ => None
      } }
    def escape(sb: StringBuilder): Unit ! R = byNext[Unit, R]("an escape: \\\" \\\\ \\/ \\b \\f \\n \\r \\t \\uXXXX") {
      case '"' => Some(take[R].map(_ => { sb.append('"'); () }))
      case '\\' => Some(take[R].map(_ => { sb.append('\\'); () }))
      case '/' => Some(take[R].map(_ => { sb.append('/'); () }))
      case 'b' => Some(take[R].map(_ => { sb.append('\b'); () }))
      case 'f' => Some(take[R].map(_ => { sb.append('\f'); () }))
      case 'n' => Some(take[R].map(_ => { sb.append('\n'); () }))
      case 'r' => Some(take[R].map(_ => { sb.append('\r'); () }))
      case 't' => Some(take[R].map(_ => { sb.append('\t'); () }))
      case 'u' => Some(take[R].flatMap(_ => hex4).map(ch => { sb.append(ch); () }))
      case _ => None
    }
    def hex4: Char ! R = satisfy[R](isHex, "a hex digit").flatMap(h1 => satisfy[R](isHex, "a hex digit").flatMap(h2 =>
      satisfy[R](isHex, "a hex digit").flatMap(h3 => satisfy[R](isHex, "a hex digit").map(h4 => Integer.parseInt(s"$h1$h2$h3$h4", 16).toChar))))
    def number: Json ! R =
      takeIf[R]("-").flatMap { neg =>
        integer.flatMap { int =>
          takeIf[R](".").flatMap(dot => if (dot) takeWhile1[R](isDigit, "a digit after the point").map("." + _) else pure[String, R](""))
            .flatMap { frac => peek[R].flatMap {
              case Some(ch) if ch == 'e' || ch == 'E' => take[R].flatMap(_ => takeWhile[R](ch => ch == '+' || ch == '-')).flatMap(sign =>
                if (sign.length > 1) fail[String, R]("a digit") else takeWhile1[R](isDigit, "a digit of the exponent").map("e" + sign + _))
              case _ => pure[String, R]("")
            }.map(exp => Json.Num(BigDecimal((if (neg) "-" else "") + int + frac + exp)): Json) }
        }
      }
    /** the integer part: a single zero, or digits not starting with one */
    def integer: String ! R = takeIf[R]("0").flatMap { zero =>
      if (zero) peek[R].flatMap { case Some(ch) if isDigit(ch) => fail[String, R]("no leading zero"); case _ => pure[String, R]("0") }
      else takeWhile1[R](isDigit, "a digit")
    }
    at(0)
  }

  // ---- the effect and its handlers
  /** a text parsed, by whichever handler is outside */
  def parseJson[R[_]](text: String, maxDepth: Int = 1000)(implicit in: In[JsonParser, R]): Either[ParseError, Json] ! R =
    perform[JsonParser].apply[R, Either[ParseError, Json]](JsonParser.Parse[Either[ParseError, Json]](text, maxDepth, e => e))
  /** by recursive descent, answered in place: the fast one */
  def descent: DescentHandler = new DescentHandler
  final class DescentHandler {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[JsonParser, R, Rest]): A ! Rest =
      handle[JsonParser, R, Rest, A, A](p, new TailHandler[JsonParser, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: JsonParser[X]): X = op match { case JsonParser.Parse(text, maxDepth, answer) => answer(Descent.parse(text, maxDepth)) }
      })
  }
  /** by the grammar on effects: the clause a program, run where the operation is */
  def grammar: GrammarHandler = new GrammarHandler
  final class GrammarHandler {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[JsonParser, R, Rest]): A ! Rest =
      handle[JsonParser, R, Rest, A, A](p, new Handler[JsonParser, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: JsonParser[X], k: X => A ! Rest) = op match {
          case JsonParser.Parse(text, maxDepth, answer) => parser(text, maxDepth).into[Rest].flatMap(e => k(answer(e)))
        }
      })
  }

  // ---- the way out
  /** a value written, wherever the handler outside writes to */
  def writeJson[R[_]](j: Json)(implicit in: In[JsonWriter, R]): Unit ! R =
    perform[JsonWriter].apply[R, Unit](JsonWriter.Write[Unit](j, u => u))

  /** into a string: the values written, each in the format, a `separator` between them — JSON lines by default */
  def collected(format: Format = Format.Compact(), separator: String = "\n"): Collected = new Collected(format, separator)
  final class Collected(format: Format, separator: String) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[JsonWriter, R, Rest]): (String, A) ! Rest =
      handle[JsonWriter, java.lang.StringBuilder, R, Rest, A, (String, A)](p, new java.lang.StringBuilder,
        new StatefulTailHandler[JsonWriter, java.lang.StringBuilder, Rest, A, (String, A)] {
          def ret(b: java.lang.StringBuilder, a: A) = pure((b.toString, a))
          def answer[X](cell: Cell[java.lang.StringBuilder], op: JsonWriter[X]): X = op match {
            case JsonWriter.Write(j, answer) =>
              val b = cell.get; if (b.length > 0) b.append(separator); b.append(format.render(j)); answer(())
          }
        })
  }

  /** to a SINK of the row outside — a console's line, a socket's send, a file's append, any program taking the text:
   * each write's text goes to it, then the program goes on */
  def writing[Rest[_]](sink: String => Unit ! Rest, format: Format = Format.Compact()): Writing[Rest] = new Writing[Rest](sink, format)
  final class Writing[Rest[_]](sink: String => Unit ! Rest, format: Format) {
    def apply[R[_], A](p: A ! R)(implicit sp: Split[JsonWriter, R, Rest]): A ! Rest =
      handle[JsonWriter, R, Rest, A, A](p, new Handler[JsonWriter, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: JsonWriter[X], k: X => A ! Rest) = op match {
          case JsonWriter.Write(j, answer) => sink(format.render(j)).flatMap(_ => k(answer(())))
        }
      })
  }

  /** as a stream of the texts written, each value one element, pulled as the program writes. The program is lifted
   * into its row with `Yield` added, so that the handler can emit; both witnesses are found for a concrete row */
  def streamed[Rest[_]](format: Format = Format.Compact()): Streamed[Rest] = new Streamed[Rest](format)
  final class Streamed[Rest[_]](format: Format) {
    type Y[Z] = Yield[String, Z]
    type RY[X] = okay.effects.Sum[Rest, Y, X]
    def apply[R[_], A](p: A ! R)(implicit sub: Sub[R, ({ type L[X] = okay.effects.Sum[R, Y, X] })#L],
        sp: Split[JsonWriter, ({ type L[X] = okay.effects.Sum[R, Y, X] })#L, RY]): streams.Stream[String, Rest] = {
      type RL[X] = okay.effects.Sum[R, Y, X]
      val emitting: A ! RY = handle[JsonWriter, RL, RY, A, A](p.into[RL], new Handler[JsonWriter, RY, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: JsonWriter[X], k: X => A ! RY) = op match {
          case JsonWriter.Write(j, answer) => emit[String, RY](format.render(j)).flatMap(_ => k(answer(())))
        }
      })
      streams.fromGenerator[String].apply[RY, Rest, A](emitting)
    }
  }

  /** a value as a stream of its pieces, compact, as they are written — the dual of `values`: a value too big for one
   * string goes out as it is walked, on the heap, however deep */
  def chunks[Rest[_]](j: Json, ascii: Boolean = false): streams.Stream[String, Rest] = {
    type Y[X] = Yield[String, X]
    type RY[X] = okay.effects.Sum[Rest, Y, X]
    def out(s: String): Unit ! RY = emit[String, RY](s)
    def quoted(s: String): String = Json.print(Json.Str(s), ascii)
    def walk(v: Json): Unit ! RY = v match {
      case Json.Arr(items) if items.nonEmpty =>
        items.iterator.zipWithIndex.foldLeft(out("[")) { case (acc, (x, i)) => acc.flatMap(_ => if (i > 0) out(",").flatMap(_ => walk(x)) else walk(x)) }.flatMap(_ => out("]"))
      case Json.Obj(fields) if fields.nonEmpty =>
        fields.iterator.zipWithIndex.foldLeft(out("{")) { case (acc, ((k, x), i)) =>
          acc.flatMap(_ => out((if (i > 0) "," else "") + quoted(k) + ":")).flatMap(_ => walk(x)) }.flatMap(_ => out("}"))
      case other => out(Json.print(other, ascii))
    }
    streams.fromGenerator[String].apply[RY, Rest, Unit](walk(j))
  }
  /** JSON LINES out: a value a line, each printed compact, the newline on */
  def toLines[Rest[_]](values: streams.Stream[Json, Rest], ascii: Boolean = false): streams.Stream[String, Rest] =
    values.map(j => Json.print(j, ascii) + "\n")

  /** a stream of chunks as a stream of its LINES, cut at `\n` wherever a chunk boundary falls, a trailing `\r` dropped;
   * the last line with no newline is a line too */
  def splitLines[Rest[_]](chunks: streams.Stream[String, Rest]): streams.Stream[String, Rest] = {
    type Y[X] = Yield[String, X]
    type RY[X] = okay.effects.Sum[Rest, Y, X]
    def line(s: String): String = if (s.endsWith("\r")) s.dropRight(1) else s
    def loop(s: streams.Stream[String, RY], carry: String): Unit ! RY = s.pull().flatMap(_.fold(
      if (carry.nonEmpty) emit[String, RY](line(carry)) else pure[Unit, RY](()))(
      (chunk, rest) => {
        val parts = (carry + chunk).split("\n", -1)
        parts.init.foldLeft(pure[Unit, RY](()))((acc, l) => acc.flatMap(_ => emit[String, RY](line(l)))).flatMap(_ => loop(rest, parts.last))
      }))
    streams.fromGenerator[String].apply[RY, Rest, Unit](loop(fromStreams.lift[Rest, RY](chunks), ""))
  }

  /** a line that did not parse: which, and why */
  final case class LineError(line: Long, error: ParseError)

  /** JSON LINES in: a value a line, blank lines skipped, each line parsed on its own (by descent) — a bad line is a
   * `Left` with its number and the stream GOES ON with the next, which `values` cannot: a line is the unit that
   * resynchronises. Lines are 1-based */
  def fromLines[Rest[_]](chunks: streams.Stream[String, Rest], maxDepth: Int = 1000): streams.Stream[Either[LineError, Json], Rest] =
    splitLines(chunks).zipWithIndex.filter { case (l, _) => l.exists(!isSpace(_)) }
      .map { case (l, i) => Descent.parse(l, maxDepth).left.map(e => LineError(i + 1, e)) }

  /** the row a JSON parser runs in: the three effects and nothing else */
  type P[X] = okay.effects.Sum[({ type L[x] = okay.effects.Sum[({ type M[y] = okay.effects.Sum[okay.effects.Void, Choose, y] })#M, Expect, x] })#L, Input, X]

  /** a text parsed by the grammar: the value, or where it failed and what was expected */
  def parser(text: String, maxDepth: Int = 1000): Either[ParseError, Json] ! okay.effects.Void = parse(text)(value[P](maxDepth))

  /** the values of a stream of chunks, as they come, whitespace between them (JSON lines, or values concatenated):
   * each a `Right`; a failure is one `Left`, the last element — the stream cannot resynchronise after it */
  def values[Rest[_]](chunks: streams.Stream[String, Rest], maxDepth: Int = 1000): streams.Stream[Either[ParseError, Json], Rest] = {
    type Y[X] = Yield[Either[ParseError, Json], X]
    type RY[X] = okay.effects.Sum[Rest, Y, X]
    type R1[X] = okay.effects.Sum[RY, Expect, X]
    type R2[X] = okay.effects.Sum[R1, Choose, X]
    type R3[X] = okay.effects.Sum[R2, Input, X]
    def loop: Unit ! R3 = skipWhile[R3](isSpace).flatMap(_ => peek[R3]).flatMap {
      case None => pure[Unit, R3](())
      case Some(_) => value[R3](maxDepth).flatMap(j => emit[Either[ParseError, Json], R3](Right(j))).flatMap(_ => loop)
    }
    // the input handler's outside is R2: the chunks, over `Rest`, lifted into it
    val lifted: streams.Stream[String, R2] = fromStreams.lift[Rest, R2](chunks)
    val parsed: Either[ParseError, Unit] ! RY =
      parsers.expectations(first(fromStreams.inputFrom[R2](lifted)(loop))).map(_.left.map { case (o, ws) => ParseError(o, 0, 0, ws, None) })
    streams.fromGenerator[Either[ParseError, Json]].apply[RY, Rest, Unit](parsed.flatMap {
      case Left(err) => emit[Either[ParseError, Json], RY](Left(err))
      case Right(()) => pure[Unit, RY](())
    })
  }
}

object Jsons {
  def apply()(implicit fx: okay.effects.Effects): Jsons[fx.type, Streams[fx.type]] = {
    implicit val exact: fx.type = fx
    new Jsons[fx.type, Streams[fx.type]](new Streams[fx.type])
  }
}
