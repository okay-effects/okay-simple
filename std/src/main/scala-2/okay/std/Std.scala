package okay.std

import okay.api._

/**
 * THE STANDARD EFFECTS' OPERATIONS AND HANDLERS, written once on the interface: a trait over any implementation,
 * mixed in where an implementation is offered — `val Simple: Std = new Machine with Std` — not into the
 * implementation itself, which knows nothing of them. So `import Simple._` gives everything: the interface's types
 * and primitives, the effects (`State`, `Reader`, `Writer`, `Error`, `Choose`) and their operations and handlers.
 * Each handler takes its effect from anywhere in the row; the parameter named, the rest inferred: `writer[String](p)`.
 * A handler answering in place is the implementation's `handleTail` / `handleStateful`: answered its own way.
 */
trait Std extends Effects {
  type State[S, X] = Standard.State[S, X]
  val State: Standard.State.type = Standard.State
  type Reader[E, X] = Standard.Reader[E, X]
  val Reader: Standard.Reader.type = Standard.Reader
  type Writer[W, X] = Standard.Writer[W, X]
  val Writer: Standard.Writer.type = Standard.Writer
  type Error[E, X] = Standard.Error[E, X]
  val Error: Standard.Error.type = Standard.Error
  type Choose[X] = Standard.Choose[X]
  val Choose: Standard.Choose.type = Standard.Choose

  def get[S, R](implicit in: In[State % S, R]): S ! R = perform[State % S].apply[R, S](Standard.State.Get(s => s))
  def put[S, R](s: S)(implicit in: In[State % S, R]): Unit ! R = perform[State % S].apply[R, Unit](Standard.State.Put(s, u => u))
  def modify[S, R](f: S => S)(implicit in: In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R](implicit in: In[Reader % E, R]): E ! R = perform[Reader % E].apply[R, E](Standard.Reader.Ask(e => e))
  def tell[W, R](w: W)(implicit in: In[Writer % W, R]): Unit ! R = perform[Writer % W].apply[R, Unit](Standard.Writer.Tell(w, u => u))
  def raise[E, R, X](e: E)(implicit in: In[Error % E, R]): X ! R = perform[Error % E].apply[R, X](Standard.Error.Raise(e))
  def among[X, R](xs: List[X])(implicit in: In[*[Choose], R]): X ! R = perform[*[Choose]].apply[R, X](Standard.Choose.Among(xs))
  def fail[X, R](implicit in: In[*[Choose], R]): X ! R = among[X, R](Nil)

  /** the final state and the value; a `k` resumed twice resumes the state of its time. Answered in place */
  def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
  final class StateHandler[S](s0: S) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[State % S, R, Rest]): (S, A) ! Rest =
      handleStateful[State % S, S, R, Rest, A, (S, A)](p)(s0)(new StatefulHandler[State % S, S, Rest, A, (S, A)] {
        def ret(s: S, a: A) = pure((s, a))
        def answer[X](cell: Cell[S], op: Op[State % S, X]): X = op match {
          case Standard.State.Get(answer) => answer(cell.get)
          case Standard.State.Put(s1, answer) => cell.set(s1); answer(())
        }
      })
  }

  /** answered in place */
  def reader[E](e: E): ReaderHandler[E] = new ReaderHandler(e)
  final class ReaderHandler[E](e: E) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Reader % E, R, Rest]): A ! Rest =
      handleTail[Reader % E, R, Rest, A, A](p)(new TailHandler[Reader % E, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Op[Reader % E, X]): X = op match { case Standard.Reader.Ask(answer) => answer(e) }
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
            case Standard.Writer.Tell(w, answer) => cell.set(w :: cell.get); answer(())
          }
        })
  }

  /** the value, or what was raised: the rest dropped */
  def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
  final class ErrorHandler[E] {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Error % E, R, Rest]): Either[E, A] ! Rest =
      handle[Error % E, R, Rest, A, Either[E, A]](p)(new Handler[Error % E, Rest, A, Either[E, A]] {
        def ret(a: A) = pure(Right(a))
        def op[X](op: Op[Error % E, X], k: X => Either[E, A] ! Rest) = op match { case Standard.Error.Raise(e) => pure(Left(e)) }
      })
  }

  /** every way the program can go: `k` resumed once for each choice */
  def all[R, Rest, A](p: A ! R)(implicit sp: Split[*[Choose], R, Rest]): List[A] ! Rest =
    handle[*[Choose], R, Rest, A, List[A]](p)(new Handler[*[Choose], Rest, A, List[A]] {
      def ret(a: A) = pure(List(a))
      def op[X](op: Op[*[Choose], X], k: X => List[A] ! Rest) = op match {
        case Standard.Choose.Among(xs) => xs.foldLeft(pure[List[A], Rest](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _)))
      }
    })
}
