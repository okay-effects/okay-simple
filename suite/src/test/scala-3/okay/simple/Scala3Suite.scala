package okay.simple

import okay.simple.machine.MachineEffects

/** Scala 3's conveniences over the same interface: no parentheses in a row, context bounds, a handler of `F Of S`,
 * an effect of one's own whose clauses Scala 3's GADT inference types with no `answer` */
class Scala3Suite extends munit.FunSuite {
  val fx = MachineEffects.effects
  import fx.*

  type Row = Reader % String + State % Int

  def greet[R: {Has[Reader % String]#L, Has[State % Int]#L}]: Int ! R =
    for { name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + name.length) } yield n

  /** an effect of one's own: its operations an enum, named `*[Counter]` in a row */
  enum Counter[X]:
    case Tick extends Counter[Unit]
    case Peek extends Counter[Int]

  def tick[R: Has[*[Counter]]#L]: Unit ! R = perform[*[Counter]].apply[R, Unit](Counter.Tick)
  def peek[R: Has[*[Counter]]#L]: Int ! R = perform[*[Counter]].apply[R, Int](Counter.Peek)

  /** its clauses typed by Scala 3's GADT inference: no `answer` in the operations */
  def counting[R, Rest, A](p: A ! R)(using Split[*[Counter], R, Rest]): (Int, A) ! Rest =
    handle[*[Counter], R, Rest, A, Int => (Int, A) ! Rest](p)(new Handler[*[Counter], Rest, A, Int => (Int, A) ! Rest]:
      def ret(a: A) = pure(n => pure((n, a)))
      def op[X](op: Op[*[Counter], X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match
        case Counter.Tick => pure(n => k(()).flatMap(f => f(n + 1)))
        case Counter.Peek => pure(n => k(n).flatMap(f => f(n)))
    ).flatMap(f => f(0))

  test("a row with no parentheses, a program by context bounds") {
    assertEquals(run(state(1)(reader("Ann")(greet[Row]))), (4, 1))
  }

  test("an effect of one's own, its clauses typed by GADT inference") {
    type C = *[Counter] + State % Int
    val p: Int ! C = tick[C].andThen(tick[C]).andThen(peek[C]).flatMap(n => put[Int, C](n * 10).as(n))
    assertEquals(run(state(0)(counting(p))), (20, (2, 2)))
  }

  test("a handler of an effect with a parameter, by its name") {
    val h = new Handler[Reader % String, Pure, Int, Int]:
      def ret(a: Int) = pure(a)
      def op[X](op: Op[Reader % String, X], k: X => Int ! Pure) = op match
        case Reader.Ask(answer) => k(answer("abc"))
    assertEquals(run(handleLast(ask[String, Pure + Reader % String].map(_.length))(h)), 3)
  }
}
