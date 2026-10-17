package okay.api

import okay.std.Std

/** programs written on the interface, Scala 3's: whatever implementation `fx` is */
class Programs(val fx: Std):
  import fx.*

  /** row-polymorphic: in any row that has these, at any place */
  def greet[R[_]: {Has[Reader % String], Has[Writer % String], Has[State % Int]}]: Int ! R =
    for
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + name.length)
    yield n

  type Row = Reader % String + Writer % String + State % Int
  type Reordered = State % Int + Reader % String + Writer % String

  def handledInOrder = run(state(10)(writer[String](reader("Ann")(greet[Row]))))
  def handledOutOfOrder = run(reader("Ann")(state(10)(writer[String](greet[Row]))))
  def anotherRow = run(writer[String](reader("Ann")(state(10)(greet[Reordered]))))

  /** a program of a smaller row, in a bigger one */
  def counter: Int ! State % Int = get[Int, State % Int]
  def grown = run(reader("x")(writer[String](state(5)(counter.into[Row]))))

  type Choosing = Choose + State % Int
  def pick: Int ! Choosing =
    for x <- among[Int, Choosing](List(1, 2, 3)); _ <- modify[Int, Choosing](_ + x) yield x * 10
  /** the state outside the choice: one state through every branch */
  def stateOutside = run(state(0)(all(pick)))
  /** the state inside: each branch its own, from the state of its time */
  def stateInside = run(all(state(0)(pick)))

  type Failing = Error % String + State % Int
  def safeDiv(a: Int, b: Int): Int ! Failing =
    if b == 0 then raise[String, Failing, Int]("div by zero") else modify[Int, Failing](_ + 1).as(a / b)
  def divided(b: Int) = run(state(0)(attempt[String](safeDiv(10, b))))
  def stateKeptOnRaise = run(attempt[String](state(0)(safeDiv(10, 0))))

  /** a split on its own, then a general handler of the effect now last; the rest of a row of two, one taken, is
   * `Pure + E` */
  type RS = Reader % String + State % Int
  def asked: Int ! RS = ask[String, RS].flatMap(n => get[Int, RS].map(_ + n.length))
  def splitThenHandle = run(state(1)(handleLast(split[Reader % String, RS, Pure + State % Int, Int](asked),
    new Handler[Reader % String, Pure + State % Int, Int, Int]:
      def ret(a: Int) = pure(a * 10)
      def op[X](op: Reader[String, X], k: X => Int ! (Pure + State % Int)) = op match
        case Reader.Ask(answer) => k(answer("abc"))
  )))

  /** constant stack: n operations under two handlers, n binds */
  def loop(n: Int): Unit ! Row =
    if n == 0 then pure(()) else ask[String, Row].flatMap(_ => modify[Int, Row](_ + 1)).andThen(loop(n - 1))
  def counted(n: Int) = run(writer[String](reader("x")(state(0)(loop(n)))))
  def binds(n: Int) = run((1 to n).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1)))
