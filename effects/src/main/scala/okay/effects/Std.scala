package okay.effects

/** THE STANDARD EFFECTS' DEFINITIONS, apart: in this object, so that other effects of the same names do not clash
 * with them. The trait `Std` gives them by name */
object Std {
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

  /** an effect of no parameter: `Choose` in a Scala 3 row, `*[Choose]#L` in a Scala 2 one */
  sealed trait Choose[X]
  object Choose {
    final case class Among[X](xs: List[X]) extends Choose[X]
  }
}

/**
 * THE STANDARD EFFECTS' OPERATIONS AND HANDLERS, written once on the interface, for both compilers: a trait over any
 * implementation,
 * mixed in where an implementation is offered — `val Machine: Std = new Machine with Std` — not into the
 * implementation itself, which knows nothing of them. So `import Machine._` gives everything: the interface's types
 * and primitives, the effects (`State`, `Reader`, `Writer`, `Error`, `Choose`) and their operations and handlers.
 * Each handler takes its effect from anywhere in the row; the parameter named, the rest inferred: `writer[String](p)`.
 * A handler answering in place is the implementation's `handle` of its shape: answered its own way.
 * An effect with a parameter is written here as both compilers read it, `({ type L[x] = State[S, x] })#L` — Scala 3's
 * `State % S`, Scala 2's `(State % S)#L`.
 */
trait Std extends Effects {
  type State[S, X] = Std.State[S, X]
  val State: Std.State.type = Std.State
  type Reader[E, X] = Std.Reader[E, X]
  val Reader: Std.Reader.type = Std.Reader
  type Writer[W, X] = Std.Writer[W, X]
  val Writer: Std.Writer.type = Std.Writer
  type Error[E, X] = Std.Error[E, X]
  val Error: Std.Error.type = Std.Error
  type Choose[X] = Std.Choose[X]
  val Choose: Std.Choose.type = Std.Choose

  def get[S, R[_]](implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R = perform[({ type L[x] = State[S, x] })#L].apply[R, S](Std.State.Get(s => s))
  def put[S, R[_]](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = perform[({ type L[x] = State[S, x] })#L].apply[R, Unit](Std.State.Put(s, u => u))
  def modify[S, R[_]](f: S => S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R[_]](implicit in: In[({ type L[x] = Reader[E, x] })#L, R]): E ! R = perform[({ type L[x] = Reader[E, x] })#L].apply[R, E](Std.Reader.Ask(e => e))
  def tell[W, R[_]](w: W)(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R = perform[({ type L[x] = Writer[W, x] })#L].apply[R, Unit](Std.Writer.Tell(w, u => u))
  def raise[E, R[_], X](e: E)(implicit in: In[({ type L[x] = Error[E, x] })#L, R]): X ! R = perform[({ type L[x] = Error[E, x] })#L].apply[R, X](Std.Error.Raise(e))
  def among[X, R[_]](xs: List[X])(implicit in: In[Choose, R]): X ! R = perform[Choose].apply[R, X](Std.Choose.Among(xs))
  def fail[X, R[_]](implicit in: In[Choose, R]): X ! R = among[X, R](Nil)

  /** the final state and the value; a `k` resumed twice resumes the state of its time. Answered in place */
  def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
  final class StateHandler[S](s0: S) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = State[S, x] })#L, R, Rest]): (S, A) ! Rest =
      handle[({ type L[x] = State[S, x] })#L, S, R, Rest, A, (S, A)](p, s0, new StatefulTailHandler[({ type L[x] = State[S, x] })#L, S, Rest, A, (S, A)] {
        def ret(s: S, a: A) = pure((s, a))
        def answer[X](cell: Cell[S], op: State[S, X]): X = op match {
          case Std.State.Get(answer) => answer(cell.get)
          case Std.State.Put(s1, answer) => cell.set(s1); answer(())
        }
      })
  }

  /** answered in place */
  def reader[E](e: E): ReaderHandler[E] = new ReaderHandler(e)
  final class ReaderHandler[E](e: E) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Reader[E, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = Reader[E, x] })#L, R, Rest, A, A](p, new TailHandler[({ type L[x] = Reader[E, x] })#L, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Reader[E, X]): X = op match { case Std.Reader.Ask(answer) => answer(e) }
      })
  }

  /** what was told, in order, and the value. Answered in place */
  def writer[W]: WriterHandler[W] = new WriterHandler[W]
  final class WriterHandler[W] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Writer[W, x] })#L, R, Rest]): (List[W], A) ! Rest =
      handle[({ type L[x] = Writer[W, x] })#L, List[W], R, Rest, A, (List[W], A)](p, Nil,
        new StatefulTailHandler[({ type L[x] = Writer[W, x] })#L, List[W], Rest, A, (List[W], A)] {
          def ret(ws: List[W], a: A) = pure((ws.reverse, a))
          def answer[X](cell: Cell[List[W]], op: Writer[W, X]): X = op match {
            case Std.Writer.Tell(w, answer) => cell.set(w :: cell.get); answer(())
          }
        })
  }

  /** the value, or what was raised: the rest dropped */
  def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
  final class ErrorHandler[E] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): Either[E, A] ! Rest =
      handle[({ type L[x] = Error[E, x] })#L, R, Rest, A, Either[E, A]](p, new Handler[({ type L[x] = Error[E, x] })#L, Rest, A, Either[E, A]] {
        def ret(a: A) = pure(Right(a))
        def op[X](op: Error[E, X], k: X => Either[E, A] ! Rest) = op match { case Std.Error.Raise(e) => pure(Left(e)) }
      })
  }

  /** every way the program can go: `k` resumed once for each choice */
  def all[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest =
    handle[Choose, R, Rest, A, List[A]](p, new Handler[Choose, Rest, A, List[A]] {
      def ret(a: A) = pure(List(a))
      def op[X](op: Choose[X], k: X => List[A] ! Rest) = op match {
        case Std.Choose.Among(xs) => xs.foldLeft(pure[List[A], Rest](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _)))
      }
    })
}
