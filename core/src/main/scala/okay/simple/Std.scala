package okay.simple

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

/** an effect of no parameter: its name a type, its operations a GADT, `Ops` between them */
sealed trait Choose
object Choose {
  sealed trait Op[X]
  final case class Among[X](xs: List[X]) extends Op[X]
  implicit val ops: Ops.Aux[Choose, Op] = Ops.of[Choose, Op]
}

/** THE STANDARD EFFECTS' OPERATIONS AND HANDLERS, written once on the interface: any implementation has them. Each
 * handler takes its effect from anywhere in the row; the parameter named, the rest inferred: `writer[String](p)` */
trait Simple extends Effects {
  def get[S, R](implicit in: In[State % S, R]): S ! R = perform[State % S].apply[R, S](State.Get(s => s))
  def put[S, R](s: S)(implicit in: In[State % S, R]): Unit ! R = perform[State % S].apply[R, Unit](State.Put(s, u => u))
  def modify[S, R](f: S => S)(implicit in: In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R](implicit in: In[Reader % E, R]): E ! R = perform[Reader % E].apply[R, E](Reader.Ask(e => e))
  def tell[W, R](w: W)(implicit in: In[Writer % W, R]): Unit ! R = perform[Writer % W].apply[R, Unit](Writer.Tell(w, u => u))
  def raise[E, R, X](e: E)(implicit in: In[Error % E, R]): X ! R = perform[Error % E].apply[R, X](Error.Raise(e))
  def among[X, R](xs: List[X])(implicit in: In[Choose, R]): X ! R = perform[Choose].apply[R, X](Choose.Among(xs))
  def fail[X, R](implicit in: In[Choose, R]): X ! R = among[X, R](Nil)

  /** the final state and the value; a `k` resumed twice resumes the state of its time */
  def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
  final class StateHandler[S](s0: S) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[State % S, R, Rest]): (S, A) ! Rest =
      handleAny[State % S, ({ type L[x] = State[S, x] })#L, R, Rest, A, S => (S, A) ! Rest](p)(
        new HandlerOf[State, S, Rest, A, S => (S, A) ! Rest] {
          def ret(a: A) = pure(s => pure((s, a)))
          def op[X](op: State[S, X], k: X => (S => (S, A) ! Rest) ! Rest) = op match {
            case State.Get(answer) => pure(s => k(answer(s)).flatMap(f => f(s)))
            case State.Put(s1, answer) => pure(_ => k(answer(())).flatMap(f => f(s1)))
          }
        }).flatMap(f => f(s0))
  }

  def reader[E](e: E): ReaderHandler[E] = new ReaderHandler(e)
  final class ReaderHandler[E](e: E) {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Reader % E, R, Rest]): A ! Rest =
      handleAny[Reader % E, ({ type L[x] = Reader[E, x] })#L, R, Rest, A, A](p)(new HandlerOf[Reader, E, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: Reader[E, X], k: X => A ! Rest) = op match { case Reader.Ask(answer) => k(answer(e)) }
      })
  }

  /** what was told, in order, and the value */
  def writer[W]: WriterHandler[W] = new WriterHandler[W]
  final class WriterHandler[W] {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Writer % W, R, Rest]): (List[W], A) ! Rest =
      handleAny[Writer % W, ({ type L[x] = Writer[W, x] })#L, R, Rest, A, (List[W], A)](p)(
        new HandlerOf[Writer, W, Rest, A, (List[W], A)] {
          def ret(a: A) = pure((Nil, a))
          def op[X](op: Writer[W, X], k: X => (List[W], A) ! Rest) = op match {
            case Writer.Tell(w, answer) => k(answer(())).map { case (ws, a) => (w :: ws, a) }
          }
        })
  }

  /** the value, or what was raised: the rest dropped */
  def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
  final class ErrorHandler[E] {
    def apply[R, Rest, A](p: A ! R)(implicit sp: Split[Error % E, R, Rest]): Either[E, A] ! Rest =
      handleAny[Error % E, ({ type L[x] = Error[E, x] })#L, R, Rest, A, Either[E, A]](p)(
        new HandlerOf[Error, E, Rest, A, Either[E, A]] {
          def ret(a: A) = pure(Right(a))
          def op[X](op: Error[E, X], k: X => Either[E, A] ! Rest) = op match { case Error.Raise(e) => pure(Left(e)) }
        })
  }

  /** every way the program can go: `k` resumed once for each choice */
  def all[R, Rest, A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest =
    handleAny[Choose, Choose.Op, R, Rest, A, List[A]](p)(new Handler[Choose.Op, Rest, A, List[A]] {
      def ret(a: A) = pure(List(a))
      def op[X](op: Choose.Op[X], k: X => List[A] ! Rest) = op match {
        case Choose.Among(xs) => xs.foldLeft(pure[List[A], Rest](Nil))((acc, x) => acc.flatMap(l => k(x).map(l ++ _)))
      }
    })
}
