package okay.effects.parsing

import okay.effects.Effects

/**
 * PARSERS over any implementation `fx`, of two standard effects: the input left, a `State[String]`, and the
 * alternatives, `Choose`. A parser is a program in any row with both; `parse(input)(p)` runs it — the state inside
 * the choice, so each alternative tried from the input of its time, the first that parses all of it taken.
 */
class Parsing[F <: Effects](implicit val fx: F) {
  import fx._
  import fx.std._

  type Input[X] = State[String, X]

  /** the next character */
  def item[R[_]](implicit s: In[Input, R], c: In[Choose, R]): Char ! R =
    get[String, R].flatMap(in => if (in.isEmpty) fail[Char, R] else put[String, R](in.substring(1)).as(in.charAt(0)))
  def satisfy[R[_]](ok: Char => Boolean)(implicit s: In[Input, R], c: In[Choose, R]): Char ! R =
    item[R].flatMap(ch => guard[R](ok(ch)).as(ch))
  def char[R[_]](ch: Char)(implicit s: In[Input, R], c: In[Choose, R]): Char ! R = satisfy[R](_ == ch)
  def string[R[_]](str: String)(implicit s: In[Input, R], c: In[Choose, R]): String ! R =
    str.foldLeft(pure[Unit, R](()))((p, ch) => p.flatMap(_ => char[R](ch).as(()))).as(str)
  /** `p`, or else `q` */
  def orElse[A, R[_]](p: => A ! R, q: => A ! R)(implicit c: In[Choose, R]): A ! R =
    among[Boolean, R](List(true, false)).flatMap(first => if (first) p else q)
  def many[A, R[_]](p: A ! R)(implicit c: In[Choose, R]): List[A] ! R = orElse[List[A], R](many1[A, R](p), pure[List[A], R](Nil))
  def many1[A, R[_]](p: A ! R)(implicit c: In[Choose, R]): List[A] ! R = p.flatMap(a => many[A, R](p).map(a :: _))
  /** the end of the input */
  def eof[R[_]](implicit s: In[Input, R], c: In[Choose, R]): Unit ! R = get[String, R].flatMap(in => guard[R](in.isEmpty))

  /** the first way `p` parses all of `input` */
  def parse(input: String): ParseHandler = new ParseHandler(input)
  final class ParseHandler(input: String) {
    def apply[R[_], R1[_], Rest[_], A](p: A ! R)(implicit st: Split[Input, R, R1], ch: Split[Choose, R1, Rest],
        c: In[Choose, R]): Option[A] ! Rest =
      first(state[String](input)(p.flatMap(a => eof[R].as(a)))).map(_.map(_._2))
  }
}

object Parsing {
  def apply()(implicit fx: Effects): Parsing[fx.type] = {
    implicit val exact: fx.type = fx
    new Parsing[fx.type]
  }
}
