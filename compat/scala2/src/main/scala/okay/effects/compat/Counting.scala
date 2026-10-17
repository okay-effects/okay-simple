package okay.effects.compat

import okay.effects._
import okay.effects.Std

/** AN EFFECT OF ONE'S OWN IN SCALA 2.13: its operations a GADT, each saying how its answer is made */
sealed trait Counter[X]
object Counter {
  final case class Tick[X](answer: Unit => X) extends Counter[X]
  final case class Peek[X](answer: Int => X) extends Counter[X]
}

/** its operations, its handler and a program — on the interface, compiled by scalac 2.13 against Scala 3's build */
class Counting(val fx: Std) {
  import fx._

  def tick[R[_]](implicit in: In[Counter, R]): Unit ! R = perform[Counter].apply[R, Unit](Counter.Tick(u => u))
  def peek[R[_]](implicit in: In[Counter, R]): Int ! R = perform[Counter].apply[R, Int](Counter.Peek(n => n))

  /** the count and the value */
  def counting[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Counter, R, Rest]): (Int, A) ! Rest =
    handle[Counter, R, Rest, A, Int => (Int, A) ! Rest](p, new Handler[Counter, Rest, A, Int => (Int, A) ! Rest] {
      def ret(a: A) = pure(n => pure((n, a)))
      def op[X](op: Counter[X], k: X => (Int => (Int, A) ! Rest) ! Rest) = op match {
        case Counter.Tick(answer) => pure(n => k(answer(())).flatMap(f => f(n + 1)))
        case Counter.Peek(answer) => pure(n => k(answer(n)).flatMap(f => f(n)))
      }
    }).flatMap(f => f(0))

  /** row-polymorphic, by Scala 2's context bounds: ticks once per name told */
  def greetAll[R[_]: Has[Counter]#L: Has[(Writer % String)#L]#L](names: List[String]): Int ! R =
    names.foldLeft(pure[Unit, R](()))((p, n) => p.andThen(tell[String, R](s"hello, $n")).andThen(tick[R]))
      .andThen(peek[R])

  /** a row named in Scala 2: by names, each `%` in parentheses, taken as its constructor */
  type Row[X] = (*[Counter] + (Writer % String))#L[X]
  def greeted(names: List[String]): (List[String], (Int, Int)) = run(writer[String](counting(greetAll[Row](names))))
}
