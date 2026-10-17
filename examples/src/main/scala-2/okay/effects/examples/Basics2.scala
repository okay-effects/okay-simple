package okay.effects.examples

import okay.effects.Effects

/**
 * THE BASICS, SCALA 2, over any implementation `fx` — `F` its type, so that code beside it, given the same `fx`, has
 * the same types: a row written by names and taken as the constructor they make, `#L` — `(Reader % String)#L`,
 * `((Reader % String) + (State % Int))#L` — each `%` in parentheses (Scala 2 has one precedence for infix types).
 * An effect's operations say how their answers are made (`answer`): Scala 2 has no GADT inference for a method's
 * type parameter, so a clause needs that to type its answer, and no cast.
 */
object Basics2 {
  /** AN EFFECT OF ONE'S OWN, Scala 2's way: each operation with its `answer` */
  sealed trait Counter[X]
  object Counter {
    final case class Tick[X](answer: Unit => X) extends Counter[X]
    final case class Peek[X](answer: Int => X) extends Counter[X]
  }
}

class Basics2[F <: Effects](val fx: F) {
  import fx._
  import fx.std._
  import Basics2.Counter

  def greet[R[_]](implicit r: In[(Reader % String)#L, R], w: In[(Writer % String)#L, R], s: In[(State % Int)#L, R]): Int ! R =
    for {
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + 1)
    } yield n

  type App[X] = ((Reader % String) + (Writer % String) + (State % Int))#L[X]

  def greeted: (Int, (List[String], Int)) = run(state(0)(writer[String](reader("Ann")(greet[App]))))

  def tick[R[_]](implicit in: In[Counter, R]): Unit ! R = perform[Counter].apply[R, Unit](Counter.Tick(u => u))
  def peek[R[_]](implicit in: In[Counter, R]): Int ! R = perform[Counter].apply[R, Int](Counter.Peek(n => n))

  def counting[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Counter, R, Rest]): (Int, A) ! Rest =
    handle[Counter, Int, R, Rest, A, (Int, A)](p, 0, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)] {
      def ret(n: Int, a: A) = pure((n, a))
      def answer[X](cell: Cell[Int], op: Counter[X]): X = op match {
        case Counter.Tick(answer) => cell.set(cell.get + 1); answer(())
        case Counter.Peek(answer) => answer(cell.get)
      }
    })

  /** the effect beside a standard one, by names: `*[Counter]` the effect of no parameter among names */
  type Ticking[X] = (*[Counter] + (State % Int))#L[X]
  def counted: (Int, (Int, Int)) =
    run(state(100)(counting(tick[Ticking].andThen(tick[Ticking]).andThen(peek[Ticking])
      .flatMap(n => modify[Int, Ticking](_ + n).as(n)))))

  /** a search, Scala 2: the first even number among the choices, times ten */
  def firstEven(xs: List[Int]): Option[Int] =
    run(first(among[Int, Choose](xs).flatMap(x => if (x % 2 == 0) pure[Int, Choose](x * 10) else fail[Int, Choose])))
}
