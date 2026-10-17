package okay.effects.examples

import okay.effects.{Runner, Sum, Void}

/**
 * CODE WRITTEN ONCE FOR BOTH COMPILERS, over any implementation `fx`, given implicitly: `new Shared()`.
 *
 *  - An effect: a GADT of its operations. Each operation says how its answer is made (`answer`), so a handler's
 *    clause needs no GADT inference — Scala 2 has none for a method's type parameter.
 *  - A row written by aliases both compilers read: `Sum[R, E, X]` is the row `R + E`, `Void` the row of no effect.
 *  - The standard effects from the implementation's `std`.
 */
sealed trait Ticket[X]
object Ticket {
  /** the next ticket's number */
  final case class Next[X](answer: Int => X) extends Ticket[X]
}

class Shared(implicit val fx: Runner) {
  import fx._
  import fx.std._

  def next[R[_]](implicit in: In[Ticket, R]): Int ! R = perform[Ticket].apply[R, Int](Ticket.Next(n => n))

  /** the handler: answering in place, its state the last number given — a ticket counter */
  def tickets[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Ticket, R, Rest]): A ! Rest =
    handle[Ticket, Int, R, Rest, A, A](p, 0, new StatefulTailHandler[Ticket, Int, Rest, A, A] {
      def ret(n: Int, a: A) = pure(a)
      def answer[X](cell: Cell[Int], op: Ticket[X]): X = op match {
        case Ticket.Next(answer) => cell.set(cell.get + 1); answer(cell.get)
      }
    })

  /** a row of two: tickets, and a writer of lines — by aliases, the same type in both compilers */
  type Lines[X] = Writer[String, X]
  type Desk[X] = Sum[({ type L[x] = Sum[Void, Lines, x] })#L, Ticket, X]

  /** in any row with tickets and lines */
  def serve[R[_]](names: List[String])(implicit t: In[Ticket, R], w: In[Lines, R]): Unit ! R =
    names.foldLeft(pure[Unit, R](())) { (p, name) =>
      p.andThen(next[R].flatMap(n => tell[String, R](s"$n: $name")))
    }

  /** the ticket handler inside, the writer outside */
  def served(names: List[String]): (List[String], Unit) = run(writer[String](tickets(serve[Desk](names))))
}
