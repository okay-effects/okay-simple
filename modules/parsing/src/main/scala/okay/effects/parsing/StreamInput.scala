package okay.effects.parsing

import okay.effects.stream.Streams

/**
 * THE INPUT FROM A STREAM OF CHUNKS: a chunk pulled when the parser needs the next character, so a text need not be in
 * memory. The handler's state — the chunk, the place in it, the stream after it — is per resume: under `orElse` each
 * alternative goes on from its own, and a chunk a rolled-back alternative needs again is pulled from the stream again,
 * so the source must REMEMBER what it gave (`fromIterator` does; a stream of effects would run them again). A parser
 * that decides by the next character never rolls back and reads each chunk once.
 */
class StreamInput[S <: Streams[_ <: okay.effects.Effects]](val streams: S) {
  import streams.fx._
  import streams.fx.std._

  /** where the parser is: the chunk and the place in it, the characters taken before this chunk, the stream after it */
  final case class At[R[_]](chunk: String, pos: Int, before: Long, rest: streams.Stream[String, R]) {
    def consumed: Long = before + pos
  }
  private val parsers = new Parsing[streams.fx.type]()(streams.fx)

  def inputFrom[Rest[_]](chunks: streams.Stream[String, Rest]): FromStream[Rest] = new FromStream[Rest](chunks)
  final class FromStream[Rest[_]](chunks: streams.Stream[String, Rest]) {
    def apply[R[_], A](p: A ! R)(implicit sp: Split[Input, R, Rest]): A ! Rest =
      handle[Input, At[Rest], R, Rest, A, A](p, At[Rest]("", 0, 0L, chunks), new StatefulHandler[Input, At[Rest], Rest, A, A] {
        def ret(s: At[Rest], a: A) = pure(a)
        /** `s` with a character at `pos`, the next chunk pulled if need be; `None` at the end of the stream */
        def fill(s: At[Rest]): Option[At[Rest]] ! Rest =
          if (s.pos < s.chunk.length) pure[Option[At[Rest]], Rest](Some(s))
          else s.rest.pull().flatMap(_.fold(pure[Option[At[Rest]], Rest](None))((c, r) => fill(At[Rest](c, 0, s.before + s.chunk.length, r))))
        /** `s` with at least `n` characters from `pos`, chunks joined as needed; fewer only at the end of the stream */
        def ensure(s: At[Rest], n: Int): At[Rest] ! Rest =
          if (s.chunk.length - s.pos >= n) pure[At[Rest], Rest](s)
          else s.rest.pull().flatMap(_.fold(pure[At[Rest], Rest](s))((c, r) => ensure(At[Rest](s.chunk.substring(s.pos) + c, 0, s.consumed, r), n)))
        def taking(s: At[Rest], ok: Char => Boolean, sb: StringBuilder): (String, At[Rest]) ! Rest =
          fill(s).flatMap {
            case None => pure[(String, At[Rest]), Rest]((sb.toString, s))
            case Some(t) =>
              var j = t.pos
              while (j < t.chunk.length && ok(t.chunk.charAt(j))) j += 1
              sb.append(t.chunk.substring(t.pos, j))
              val u = t.copy(pos = j)
              if (j < t.chunk.length) pure[(String, At[Rest]), Rest]((sb.toString, u)) else taking(u, ok, sb)
          }
        def op[X](s: At[Rest], op: Input[X], k: (X, At[Rest]) => A ! Rest): A ! Rest = op match {
          case Input.Peek(answer) => fill(s).flatMap {
            case Some(t) => k(answer(Some(t.chunk.charAt(t.pos))), t)
            case None => k(answer(None), s)
          }
          case Input.Take(answer) => fill(s).flatMap {
            case Some(t) => k(answer(Some(t.chunk.charAt(t.pos))), t.copy(pos = t.pos + 1))
            case None => k(answer(None), s)
          }
          case Input.TakeWhile(ok, answer) => taking(s, ok, new StringBuilder).flatMap { case (str, t) => k(answer(str), t) }
          case Input.TakeIf(str, answer) => ensure(s, str.length).flatMap { t =>
            if (t.chunk.startsWith(str, t.pos)) k(answer(true), t.copy(pos = t.pos + str.length)) else k(answer(false), t)
          }
          case Input.Offset(answer) => k(answer(s.consumed), s)
        }
      })
  }

  /** `p` over the chunks, all of them: the value, or the furthest failure by offset (no line and column: the text is
   * not kept). The chunks are a stream of the row outside the parser's three effects, lifted into the input handler's */
  def parseStream[Rest[_]](chunks: streams.Stream[String, Rest]): ParseStream[Rest] = new ParseStream[Rest](chunks)
  final class ParseStream[Rest[_]](chunks: streams.Stream[String, Rest]) {
    def apply[R[_], R1[_], R2[_], A](p: A ! R)(implicit si: Split[Input, R, R1], sc: Split[Choose, R1, R2], se: Split[Expect, R2, Rest],
        i: In[Input, R], e: In[Expect, R], c: In[Choose, R], sub: Sub[Rest, R1]): Either[ParseError, A] ! Rest =
      parsers.expectations(first(inputFrom[R1](lift[Rest, R1](chunks))(p.flatMap(a => parsers.eof[R].as(a)))))
        .map(_.left.map { case (o, ws) => ParseError(o, 0, 0, ws, None) })
  }
  /** a stream of a row as one of a row of more effects */
  def lift[R[_], S[_]](s: streams.Stream[String, R])(implicit sub: Sub[R, S]): streams.Stream[String, S] =
    new streams.Stream[String, S](() => s.pull().into[S].map(_.fold[streams.Pull[String, S]](new streams.End[String, S])((c, r) => new streams.Emit[String, S](c, lift[R, S](r)))))
}

object StreamInput {
  /** over the streams given, so that its streams are the caller's */
  def apply[S <: Streams[_ <: okay.effects.Effects]](streams: S): StreamInput[streams.type] = new StreamInput[streams.type](streams)
}
