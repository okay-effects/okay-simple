package okay.effects

import okay.effects.simple.Machine

/** Scala 3's interface: rows of type constructors, no parentheses and no `*`, context bounds, an effect of one's own
 * an enum whose clauses Scala 3's GADT inference types */
class Scala3Suite extends munit.FunSuite:
  import Machine.*
  import Machine.std.*

  type Row = Reader % String + State % Int

  def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: Int ! R =
    for name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + name.length) yield n

  /** an effect of one's own: its operations an enum, named by itself in a row */
  enum Counter[X]:
    case Tick extends Counter[Unit]
    case Peek extends Counter[Int]

  def tick[R[_]: Has[Counter]]: Unit ! R = perform[Counter](Counter.Tick)
  def peek[R[_]: Has[Counter]]: Int ! R = perform[Counter](Counter.Peek)

  /** its clauses typed by Scala 3's GADT inference: no `answer` in the operations */
  def counting[R[_], Rest[_], A](p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
    handle[Counter, R, Rest, A, Int => (Int, A) ! Rest](p, new Handler[Counter, Rest, A, Int => (Int, A) ! Rest]:
      def ret(a: A) = pure(n => pure((n, a)))
      def op[X](op: Counter[X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match
        case Counter.Tick => pure(n => k(()).flatMap(f => f(n + 1)))
        case Counter.Peek => pure(n => k(n).flatMap(f => f(n)))
    ).flatMap(f => f(0))

  test("a row with no parentheses, a program by context bounds"):
    assertEquals(run(state(1)(reader("Ann")(greet[Row]))), (4, 1))

  test("an effect of one's own, its clauses typed by GADT inference"):
    type C = Counter + State % Int
    val p: Int ! C = tick[C].andThen(tick[C]).andThen(peek[C]).flatMap(n => put[Int, C](n * 10).as(n))
    assertEquals(run(state(0)(counting(p))), (20, (2, 2)))

  test("a handler of an effect with a parameter: its operations as they are"):
    val h = new Handler[Reader % String, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def op[X](op: Reader[String, X], k: X => Int ! Pure) = op match
        case Reader.Ask(answer) => k(answer("abc"))
    assertEquals(run(handleLast(ask[String, Pure + Reader % String].map(_.length), h)), 3)

  test("`*[F]` is `F`: code written alike for both compilers still compiles"):
    def among3[R[_]: Has[*[Choose]]]: Int ! R = among[Int, R](List(1, 2, 3))
    assertEquals(run(state(0)(all(among3[*[Choose] + State % Int]))), (0, List(1, 2, 3)))
