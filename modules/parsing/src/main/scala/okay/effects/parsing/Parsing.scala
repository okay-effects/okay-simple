package okay.effects.parsing

import okay.effects.Effects

/** THE INPUT: a cursor over characters. Its handler's business where they come from — a text in memory (`input`,
 * answered in place, an index into the text, no copy) or chunks pulled from a stream (`StreamInput`). `TakeWhile` and
 * `TakeIf` are operations so that a token costs one operation, not one per character */
sealed trait Input[X]
object Input {
  /** the next character, not taken */
  final case class Peek[X](answer: Option[Char] => X) extends Input[X]
  /** the next character, taken */
  final case class Take[X](answer: Option[Char] => X) extends Input[X]
  /** the characters while `ok`, taken; possibly none */
  final case class TakeWhile[X](ok: Char => Boolean, answer: String => X) extends Input[X]
  /** `s`, if it is next: taken, and `true`; else nothing taken */
  final case class TakeIf[X](s: String, answer: Boolean => X) extends Input[X]
  /** the characters taken so far */
  final case class Offset[X](answer: Long => X) extends Input[X]
}

/** WHAT A PARSER EXPECTED where it failed. Handled OUTSIDE the choice, so the furthest failure survives backtracking:
 * the error reported is the furthest point reached and everything expected there. And, for a committed choice
 * (`orCommitted`), what an attempt did before the choice rolled it back: an attempt is OPENED where it starts, CLOSED
 * when it succeeds, and ASKED by the other alternative whether it was committed — it succeeded, or failed past its
 * start */
sealed trait Expect[X]
object Expect {
  final case class Failed[X](offset: Long, expected: String, answer: Unit => X) extends Expect[X]
  /** an attempt opened at `start`: its id */
  final case class Open[X](start: Long, answer: Long => X) extends Expect[X]
  /** the attempt succeeded */
  final case class Close[X](id: Long, answer: Unit => X) extends Expect[X]
  /** whether the attempt committed: it succeeded, or it failed having gone past its start */
  final case class Committed[X](id: Long, answer: Boolean => X) extends Expect[X]
}

/** where a parse failed and what was expected there; `line` and `column` are 1-based, 0 when unknown (a stream) */
final case class ParseError(offset: Long, line: Int, column: Int, expected: List[String], found: Option[String]) {
  def message: String = {
    val where = if (line > 0) s"line $line, column $column" else s"offset $offset"
    val what = if (expected.isEmpty) "no parse" else s"expected ${expected.mkString(" or ")}"
    s"at $where: $what" + found.fold("")(f => s", found $f")
  }
}
object ParseError {
  /** the error at `offset` of `text`: its line and column, what was found there */
  def at(text: String, offset: Long, expected: List[String]): ParseError = {
    val o = math.max(0L, math.min(offset, text.length.toLong)).toInt
    var line = 1; var col = 1; var i = 0
    while (i < o) { if (text.charAt(i) == '\n') { line += 1; col = 1 } else col += 1; i += 1 }
    val found = if (o < text.length) Some(s"'${text.charAt(o)}'") else Some("end of input")
    ParseError(o.toLong, line, col, expected, found)
  }
}

/**
 * PARSERS over any implementation `fx`: programs of three effects — the `Input`, the `Expect`ations, and the
 * alternatives, `Choose`. A parser that decides by the next character (`byNext`, `whileNext`, `takeWhile`) never
 * makes a choice point and runs as a straight line of operations answered in place; `orElse` and `many` backtrack
 * by `Choose`, each alternative from the input of its time. `parse(text)(p)` runs a parser over a text:
 * `Right(a)` when all of it parsed, `Left(error)` saying where it failed and what was expected there.
 */
class Parsing[F <: Effects with Singleton](implicit implementation: F) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation to the code it is imported into */
  val fx: F = implementation
  import fx._
  import fx.std._

  // ---- the input's words
  def peek[R[_]](implicit in: In[Input, R]): Option[Char] ! R = perform[Input].apply[R, Option[Char]](Input.Peek[Option[Char]](c => c))
  def take[R[_]](implicit in: In[Input, R]): Option[Char] ! R = perform[Input].apply[R, Option[Char]](Input.Take[Option[Char]](c => c))
  def takeWhile[R[_]](ok: Char => Boolean)(implicit in: In[Input, R]): String ! R = perform[Input].apply[R, String](Input.TakeWhile[String](ok, s => s))
  def takeIf[R[_]](s: String)(implicit in: In[Input, R]): Boolean ! R = perform[Input].apply[R, Boolean](Input.TakeIf[Boolean](s, b => b))
  def offset[R[_]](implicit in: In[Input, R]): Long ! R = perform[Input].apply[R, Long](Input.Offset[Long](o => o))
  def skipWhile[R[_]](ok: Char => Boolean)(implicit in: In[Input, R]): Unit ! R = takeWhile[R](ok).as(())
  def spaces[R[_]](implicit in: In[Input, R]): Unit ! R = skipWhile[R](_.isWhitespace)
  def expected[R[_]](at: Long, what: String)(implicit in: In[Expect, R]): Unit ! R =
    perform[Expect].apply[R, Unit](Expect.Failed[Unit](at, what, u => u))

  /** the parser fails here, saying what it expected; under `orElse` the other alternative is tried, and the furthest
   * failure is the error reported */
  def fail[A, R[_]](what: String)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): A ! R =
    offset[R].flatMap(o => expected[R](o, what)).flatMap(_ => among[A, R](Nil))

  // ---- parsers
  /** the next character */
  def item[R[_]](implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Char ! R =
    take[R].flatMap { case Some(ch) => pure[Char, R](ch); case None => fail[Char, R]("a character") }
  /** the next character if `ok` — NOT taken otherwise, so a choice decided by it consumes nothing on the way out */
  def satisfy[R[_]](ok: Char => Boolean, what: String = "a character")(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Char ! R =
    peek[R].flatMap { case Some(ch) if ok(ch) => take[R].as(ch); case _ => fail[Char, R](what) }
  def char[R[_]](ch: Char)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Char ! R = satisfy[R](_ == ch, s"'$ch'")
  def string[R[_]](s: String)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): String ! R =
    takeIf[R](s).flatMap(ok => if (ok) pure[String, R](s) else fail[String, R](s"'$s'"))
  /** one or more characters while `ok` */
  def takeWhile1[R[_]](ok: Char => Boolean, what: String)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): String ! R =
    takeWhile[R](ok).flatMap(s => if (s.isEmpty) fail[String, R](what) else pure[String, R](s))
  /** the end of the input */
  def eof[R[_]](implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Unit ! R =
    peek[R].flatMap { case None => pure[Unit, R](()); case Some(_) => fail[Unit, R]("end of input") }

  // ---- decided by the next character: no choice point, no backtracking
  /** the parser `cases` gives for the next character; `None`, or no character, fails saying `what` */
  def byNext[A, R[_]](what: String)(cases: Char => Option[A ! R])(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): A ! R =
    peek[R].flatMap { case Some(ch) => cases(ch).getOrElse(fail[A, R](what)); case None => fail[A, R](what) }
  /** `p` again and again while the next character is `ok` */
  def whileNext[A, R[_]](ok: Char => Boolean)(p: => A ! R)(implicit i: In[Input, R]): List[A] ! R = {
    def loop(acc: List[A]): List[A] ! R =
      peek[R].flatMap { case Some(ch) if ok(ch) => p.flatMap(a => loop(a :: acc)); case _ => pure[List[A], R](acc.reverse) }
    loop(Nil)
  }

  // ---- alternatives: backtracking by the choice
  /** `p`, or else `q` from the same input */
  def orElse[A, R[_]](p: => A ! R, q: => A ! R)(implicit c: In[Choose, R]): A ! R =
    among[Boolean, R](List(true, false)).flatMap(first => if (first) p else q)
  def many[A, R[_]](p: => A ! R)(implicit c: In[Choose, R]): List[A] ! R = orElse[List[A], R](many1[A, R](p), pure[List[A], R](Nil))
  def many1[A, R[_]](p: => A ! R)(implicit c: In[Choose, R]): List[A] ! R = p.flatMap(a => many[A, R](p).map(a :: _))
  def optional[A, R[_]](p: => A ! R)(implicit c: In[Choose, R]): Option[A] ! R =
    orElse[Option[A], R](p.map(a => Some(a): Option[A]), pure[Option[A], R](None))
  /** `p`s separated by `sep`, none or more */
  def sepBy[A, R[_]](p: => A ! R, sep: => Unit ! R)(implicit c: In[Choose, R]): List[A] ! R =
    orElse[List[A], R](sepBy1[A, R](p, sep), pure[List[A], R](Nil))
  def sepBy1[A, R[_]](p: => A ! R, sep: => Unit ! R)(implicit c: In[Choose, R]): List[A] ! R =
    p.flatMap(a => many[A, R](sep.flatMap(_ => p)).map(a :: _))

  // ---- committed alternatives: parsec's `p <|> q`
  /** `p`, or else `q` — but `q` ONLY if `p` failed having taken nothing. Once `p` went past where it started and
   * failed, the parse fails there, with `p`'s error: what was expected where the input stopped making sense, not
   * the last alternative's at the start. Once `p` succeeded, a failure after it does not come back to `q`.
   * `orElse` is parsec's `try p <|> q`, backtracking always; this is `p <|> q`. `p`'s own alternatives are as `p`
   * makes them */
  def orCommitted[A, R[_]](p: => A ! R, q: => A ! R)(
    implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): A ! R =
    offset[R].flatMap(start => perform[Expect].apply[R, Long](Expect.Open[Long](start, id => id)).flatMap(id =>
      among[Boolean, R](List(true, false)).flatMap { first =>
        if (first) p.flatMap(a => perform[Expect].apply[R, Unit](Expect.Close[Unit](id, u => u)).as(a))
        else perform[Expect].apply[R, Boolean](Expect.Committed[Boolean](id, b => b)).flatMap(committed =>
          if (committed) among[A, R](Nil) else q)
      }))
  /** `p` as many times as it parses, committed: a `p` that went past its start and failed is the parse's error */
  def manyCommitted[A, R[_]](p: => A ! R)(implicit i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): List[A] ! R =
    orCommitted[List[A], R](p.flatMap(a => manyCommitted[A, R](p).map(a :: _)), pure[List[A], R](Nil))

  // ---- running
  /** the input from a text in memory: an index into it, answered in place, no copy of the text */
  def input(text: String): InputHandler = new InputHandler(text)
  final class InputHandler(text: String) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Input, R, Rest]): A ! Rest =
      handle[Input, Int, R, Rest, A, A](p, 0, new StatefulTailHandler[Input, Int, Rest, A, A] {
        def ret(s: Int, a: A) = pure(a)
        def answer[X](cell: Cell[Int], op: Input[X]): X = op match {
          case Input.Peek(answer) =>
            val i = cell.get; answer(if (i < text.length) Some(text.charAt(i)) else None)
          case Input.Take(answer) =>
            val i = cell.get
            if (i < text.length) { cell.set(i + 1); answer(Some(text.charAt(i))) } else answer(None)
          case Input.TakeWhile(ok, answer) =>
            val i = cell.get; var j = i
            while (j < text.length && ok(text.charAt(j))) j += 1
            cell.set(j); answer(text.substring(i, j))
          case Input.TakeIf(s, answer) =>
            val i = cell.get
            if (text.startsWith(s, i)) { cell.set(i + s.length); answer(true) } else answer(false)
          case Input.Offset(answer) => answer(cell.get.toLong)
        }
      })
  }

  /** the furthest failure and everything expected there, kept outside the choice: `Left((offset, expected))` when the
   * parse gave nothing. And the open attempts of committed choices: a stack, the innermost on top, since the choice
   * goes depth first; a failure raises the top one's furthest point, a closed one hands its furthest to the one
   * below */
  def expectations: Expectations = new Expectations
  final class Expectations {
    def apply[R[_], Rest[_], A](p: Option[A] ! R)(implicit sp: Split[Expect, R, Rest]): Either[(Long, List[String]), A] ! Rest =
      handle[Expect, Expectations.State, R, Rest, Option[A], Either[(Long, List[String]), A]](p, Expectations.empty,
        new StatefulTailHandler[Expect, Expectations.State, Rest, Option[A], Either[(Long, List[String]), A]] {
          def ret(s: Expectations.State, oa: Option[A]) = pure(oa match {
            case Some(a) => Right(a)
            case None => Left((math.max(0L, s.furthest), s.expected.reverse))
          })
          def answer[X](cell: Cell[Expectations.State], op: Expect[X]): X = op match {
            case Expect.Failed(o, what, answer) =>
              val s = cell.get
              val at = if (o > s.furthest) s.copy(furthest = o, expected = List(what))
                else if (o == s.furthest && !s.expected.contains(what)) s.copy(expected = what :: s.expected) else s
              cell.set(at.copy(open = at.open match {
                case top :: below => top.copy(reached = math.max(top.reached, o)) :: below
                case Nil => Nil
              }))
              answer(())
            case Expect.Open(start, answer) =>
              val s = cell.get
              cell.set(s.copy(open = Expectations.Attempt(s.next, start, -1L) :: s.open, next = s.next + 1))
              answer(s.next)
            case Expect.Close(id, answer) =>
              cell.set(closed(cell.get, id)._1); answer(())
            case Expect.Committed(id, answer) =>
              val (s, attempt) = closed(cell.get, id)
              cell.set(s)
              // not open: it was closed by its success; open: it failed, committed if it got past its start
              answer(attempt.forall(a => a.reached > a.start))
          }
          /** the attempt `id` taken off the stack, its furthest handed to the one below */
          private def closed(s: Expectations.State, id: Long): (Expectations.State, Option[Expectations.Attempt]) =
            s.open.span(_.id != id) match {
              case (above, a :: below) =>
                val reached = (a :: above).map(_.reached).max
                (s.copy(open = below match {
                  case b :: more => b.copy(reached = math.max(b.reached, reached)) :: more
                  case Nil => Nil
                }), Some(a.copy(reached = reached)))
              case _ => (s, None)
            }
        })
  }
  object Expectations {
    /** an open attempt: where it started, the furthest any failure in it got */
    final case class Attempt(id: Long, start: Long, reached: Long)
    final case class State(furthest: Long, expected: List[String], open: List[Attempt], next: Long)
    val empty: State = State(-1L, Nil, Nil, 0L)
  }

  /** `p` over `text`, all of it: the value, or the furthest failure with its line and column */
  def parse(text: String): Parse = new Parse(text)
  final class Parse(text: String) {
    def apply[R[_], R1[_], R2[_], Rest[_], A](p: A ! R)(implicit si: Split[Input, R, R1], sc: Split[Choose, R1, R2], se: Split[Expect, R2, Rest],
        i: In[Input, R], e: In[Expect, R], c: In[Choose, R]): Either[ParseError, A] ! Rest =
      expectations(first(input(text)(p.flatMap(a => eof[R].as(a)))))
        .map(_.left.map { case (o, ws) => ParseError.at(text, o, ws) })
  }
}

object Parsing {
  def apply()(implicit fx: Effects): Parsing[fx.type] = {
    implicit val exact: fx.type = fx
    new Parsing[fx.type]
  }
}
