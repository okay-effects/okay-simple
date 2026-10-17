package okay.std

import okay.api._

/** the standard effects: each a GADT of its operations. An operation says how its answer is made, `answer`, so a
 * handler's clause needs no GADT inference — Scala 2 has none for a method's type parameter — and no cast */
sealed trait State[S, X]
object State {
  final case class Get[S, X](answer: S => X) extends State[S, X]
  final case class Put[S, X](s: S, answer: Unit => X) extends State[S, X]
}

sealed trait Reader[E, X]
object Reader {
  final case class Ask[E, X](answer: E => X) extends Reader[E, X]
}

sealed trait Writer[W, X]
object Writer {
  final case class Tell[W, X](w: W, answer: Unit => X) extends Writer[W, X]
}

sealed trait Error[E, X]
object Error {
  final case class Raise[E, X](e: E) extends Error[E, X]
}

/** an effect of no parameter, named `*[Choose]` in a row */
sealed trait Choose[X]
object Choose {
  final case class Among[X](xs: List[X]) extends Choose[X]
}

/** THE STANDARD EFFECTS' OPERATIONS AND HANDLERS, written once on the interface, over any implementation of it —
 * which knows nothing of them: `val std = Std(fx); import fx._, std._`, the types `fx`'s. Each handler takes its
 * effect from anywhere in the row; the parameter named, the rest inferred: `writer[String](p)`. A handler answering
 * in place is `fx`'s `handleTail` / `handleStateful`: an implementation answers it its own way */
final class Std[F <: Effects](val fx: F) {
  import fx._

  def get[S, R](implicit in: In[State % S, R]): S ! R = perform[State % S].apply[R, S](State.Get(s => s))
  def put[S, R](s: S)(implicit in: In[State % S, R]): Unit ! R = perform[State % S].apply[R, Unit](State.Put(s, u => u))
  def modify[S, R](f: S => S)(implicit in: In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R](implicit in: In[Reader % E, R]): E ! R = perform[Reader % E].apply[R, E](Reader.Ask(e => e))
  def tell[W, R](w: W)(implicit in: In[Writer % W, R]): Unit ! R = perform[Writer % W].apply[R, Unit](Writer.Tell(w, u => u))
  def raise[E, R, X](e: E)(implicit in: In[Error % E, R]): X ! R = perform[Error % E].apply[R, X](Error.Raise(e))
  def among[X, R](xs: List[X])(implicit in: In[*[Choose], R]): X ! R = perform[*[Choose]].apply[R, X](Choose.Among(xs))
  def fail[X, R](implicit in: In[*[Choose], R]): X ! R = among[X, R](Nil)

  /** the final state and the value; a `k` resumed twice resumes the state of its time. Answered in place */
  def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
  final class StateHandler[S](s0: S) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[State % S, R, Rest]): (S, A) ! Rest =
      handleStateful[State % S, S, R, Rest, A, (S, A)](p)(s0)(new StatefulHandler[State % S, S, Rest, A, (S, A)] {
        def ret(s: S, a: A) = pure((s, a))
        def answer[X](cell: Cell[S], op: Op[State % S, X]): X = op match {
          case State.Get(answer) => answer(cell.get)
          case State.Put(s1, answer) => cell.set(s1); answer(())
        }
      })
  }

  /** answered in place */
  def reader[E](e: E): ReaderHandler[E] = new ReaderHandler(e)
  final class ReaderHandler[E](e: E) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Reader % E, R, Rest]): A ! Rest =
      handleTail[Reader % E, R, Rest, A, A](p)(new TailHandler[Reader % E, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Op[Reader % E, X]): X = op match { case Reader.Ask(answer) => answer(e) }
      })
  }

  /** what was told, in order, and the value. Answered in place */
  def writer[W]: WriterHandler[W] = new WriterHandler[W]
  final class WriterHandler[W] {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Writer % W, R, Rest]): (List[W], A) ! Rest =
      handleStateful[Writer % W, List[W], R, Rest, A, (List[W], A)](p)(Nil)(
        new StatefulHandler[Writer % W, List[W], Rest, A, (List[W], A)] {
          def ret(ws: List[W], a: A) = pure((ws.reverse, a))
          def answer[X](cell: Cell[List[W]], op: Op[Writer % W, X]): X = op match {
            case Writer.Tell(w, answer) => cell.set(w :: cell.get); answer(())
          }
        })
  }

  /** the value, or what was raised: the rest dropped */
  def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
  final class ErrorHandler[E] {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Error % E, R, Rest]): Either[E, A] ! Rest =
      handle[Error % E, R, Rest, A, Either[E, A]](p)(new Handler[Error % E, Rest, A, Either[E, A]] {
        def ret(a: A) = pure(Right(a))
        def op[X](op: Op[Error % E, X], k: X => Either[E, A] ! Rest) = op match { case Error.Raise(e) => pure(Left(e)) }
      })
  }

  /** every way the program can go: `k` resumed once for each choice */
  def all[R, Rest, A](p: A ! R)(implicit sp: Split[*[Choose], R, Rest]): List[A] ! Rest =
    handle[*[Choose], R, Rest, A, List[A]](p)(new Handler[*[Choose], Rest, A, List[A]] {
      def ret(a: A) = pure(List(a))
      def op[X](op: Op[*[Choose], X], k: X => List[A] ! Rest) = op match {
        case Choose.Among(xs) => xs.foldLeft(pure[List[A], Rest](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _)))
      }
    })
}

object Std {
  /** the standard effects on `fx`: their programs `fx`'s */
  def apply(fx: Effects): Std[fx.type] = new Std[fx.type](fx)
}
