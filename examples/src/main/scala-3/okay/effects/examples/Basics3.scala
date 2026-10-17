package okay.effects.examples

import okay.effects.simple.Machine
import okay.effects.simple.Machine.*
import okay.effects.simple.Machine.std.*

/**
 * THE BASICS, SCALA 3: a row of type constructors (`Reader % String + State % Int`), its requirements as context
 * bounds, an effect of one's own an `enum` whose clauses GADT inference types, handlers of each shape.
 */
object Basics3:
  /** a program in any row that has these three, at any place */
  def greet[R[_]: {Has[Reader % String], Has[Writer % String], Has[State % Int]}]: Int ! R =
    for
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + 1)
    yield n

  type App = Reader % String + Writer % String + State % Int

  /** each handler takes its effect from wherever it is in the row: the order of handling is the reader's, the
   * writer's, the state's, from the inside out — the row's order is not */
  def greeted: (Int, (List[String], Int)) = run(state(0)(writer[String](reader("Ann")(greet[App]))))

  /** AN EFFECT OF ONE'S OWN: its operations an enum, the answer type in each case */
  enum Counter[X]:
    case Tick extends Counter[Unit]
    case Peek extends Counter[Int]

  def tick[R[_]: Has[Counter]]: Unit ! R = perform[Counter](Counter.Tick)
  def peek[R[_]: Has[Counter]]: Int ! R = perform[Counter](Counter.Peek)

  /** its handler, answering in place with a state of its own, in a cell: no `k` */
  def counting[R[_], Rest[_], A](p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
    handle(p, 0, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)]:
      def ret(n: Int, a: A) = pure((n, a))
      def answer[X](cell: Cell[Int], op: Counter[X]): X = op match
        case Counter.Tick => cell.set(cell.get + 1)
        case Counter.Peek => cell.get)

  /** the same effect beside a standard one: ticks and a state, one row */
  type Ticking = Counter + State % Int
  def counted: (Int, (Int, Int)) =
    run(state(100)(counting(tick[Ticking].andThen(tick[Ticking]).andThen(peek[Ticking])
      .flatMap(n => modify[Int, Ticking](_ + n).as(n)))))

  /** A GENERAL HANDLER, its clause given `k`: here `k` resumed twice — every way the program can go */
  enum Coin[X]:
    case Flip extends Coin[Boolean]

  def flip[R[_]: Has[Coin]]: Boolean ! R = perform[Coin](Coin.Flip)

  def outcomes[R[_], Rest[_], A](p: A ! R)(using Split[Coin, R, Rest]): List[A] ! Rest =
    handle(p, new Handler[Coin, Rest, A, List[A]]:
      def ret(a: A) = pure(List(a))
      def op[X](op: Coin[X], k: X => List[A] ! Rest) = op match
        case Coin.Flip => k(true).flatMap(t => k(false).map(t ++ _)))

  def twoFlips: List[(Boolean, Boolean)] = run(outcomes(for a <- flip[Coin]; b <- flip[Coin] yield (a, b)))
