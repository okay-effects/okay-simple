package okay.effects.json

import okay.effects.parsing.{Expect, Input, ParseError, Parsing, StreamInput}
import okay.effects.stream.Streams

/**
 * JSON PARSED BY EFFECTS: the grammar a program of the parsing module's three effects, decided by the next character —
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

  /** the row a JSON parser runs in: the three effects and nothing else */
  type P[X] = okay.effects.Sum[({ type L[x] = okay.effects.Sum[({ type M[y] = okay.effects.Sum[okay.effects.Void, Choose, y] })#M, Expect, x] })#L, Input, X]

  /** a text parsed: the value, or where it failed and what was expected */
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
