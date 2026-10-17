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

  /** A PROGRAM OF ANOTHER MONAD'S, as an effect: `Of(ga)`, a `G[X]` performed as it is, its value the answer — `IO`,
   * a ZIO task, an `Option`, a `Future`, anything `lower` can bind (`Lowering`) */
  sealed trait Lift[G[_], X]
  object Lift {
    final case class Of[G[_], X](ga: G[X]) extends Lift[G, X]
  }
  /** what `lower` needs of `G`: its `pure` and its bind — a monad, cats' or ZIO's or one's own, with no library
   * named here (the interop modules give it from theirs) */
  trait Lowering[G[_]] {
    def pure[A](a: A): G[A]
    def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
  }

  /** a generator's: `Emit(o)`, a value given out, the generator going on */
  sealed trait Yield[O, X]
  object Yield {
    final case class Emit[O, X](o: O, answer: Unit => X) extends Yield[O, X]
  }

  /** DELIMITED CONTROL: `Shift(f)`, `f` given the rest of the program up to the nearest `reset`, of the answer program
   * `P` (`B ! R`) */
  sealed trait Control[P, X]
  object Control {
    final case class Shift[P, X](f: (X => P) => P) extends Control[P, X]
  }
}

/**
 * THE STANDARD EFFECTS' OPERATIONS AND HANDLERS, written once on the interface, for both compilers: over any
 * implementation `fx`, apart from it — the implementation knows nothing of them, and offers them as its `std`, a
 * member of the interface. So two imports, the implementation's and its standard effects':
 * `import Machine._; import Machine.std._`. The effects (`State`, `Reader`, `Writer`, `Error`, `Choose`, `Control` for shift and reset), their
 * operations and their handlers, each handler taking its effect from anywhere in the row; the parameter named, the
 * rest inferred: `writer[String](p)`. A handler answering in place is the implementation's `handle` of its shape.
 * An effect with a parameter is written here as both compilers read it, `({ type L[x] = State[S, x] })#L` — Scala 3's
 * `State % S`, Scala 2's `(State % S)#L`.
 */
class Std[F <: Effects](val fx: F) {
  import fx._

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
  type Control[P, X] = Std.Control[P, X]
  val Control: Std.Control.type = Std.Control
  type Yield[O, X] = Std.Yield[O, X]
  val Yield: Std.Yield.type = Std.Yield
  type Lift[G[_], X] = Std.Lift[G, X]
  val Lift: Std.Lift.type = Std.Lift
  type Lowering[G[_]] = Std.Lowering[G]

  def get[S, R[_]](implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R = perform[({ type L[x] = State[S, x] })#L].apply[R, S](Std.State.Get(s => s))
  def put[S, R[_]](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = perform[({ type L[x] = State[S, x] })#L].apply[R, Unit](Std.State.Put(s, u => u))
  def modify[S, R[_]](f: S => S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R[_]](implicit in: In[({ type L[x] = Reader[E, x] })#L, R]): E ! R = perform[({ type L[x] = Reader[E, x] })#L].apply[R, E](Std.Reader.Ask(e => e))
  def tell[W, R[_]](w: W)(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R = perform[({ type L[x] = Writer[W, x] })#L].apply[R, Unit](Std.Writer.Tell(w, u => u))
  def raise[E, R[_], X](e: E)(implicit in: In[({ type L[x] = Error[E, x] })#L, R]): X ! R = perform[({ type L[x] = Error[E, x] })#L].apply[R, X](Std.Error.Raise(e))
  def among[X, R[_]](xs: List[X])(implicit in: In[Choose, R]): X ! R = perform[Choose].apply[R, X](Std.Choose.Among(xs))
  def fail[X, R[_]](implicit in: In[Choose, R]): X ! R = among[X, R](Nil)
  def lift[G[_], X, R[_]](ga: G[X])(implicit in: In[({ type L[x] = Lift[G, x] })#L, R]): X ! R =
    perform[({ type L[x] = Lift[G, x] })#L].apply[R, X](Std.Lift.Of(ga))
  def emit[O, R[_]](o: O)(implicit in: In[({ type L[x] = Yield[O, x] })#L, R]): Unit ! R = perform[({ type L[x] = Yield[O, x] })#L].apply[R, Unit](Std.Yield.Emit(o, u => u))

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

  /** the value, or what was raised: the rest dropped — a handler aborting, no `k` */
  def attempt[E]: ErrorHandler[E] = new ErrorHandler[E]
  final class ErrorHandler[E] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): Either[E, A] ! Rest =
      handle[({ type L[x] = Error[E, x] })#L, R, Rest, A, Either[E, A]](p, new AbortHandler[({ type L[x] = Error[E, x] })#L, Rest, A, Either[E, A]] {
        def ret(a: A) = pure(Right(a))
        def abort[X](op: Error[E, X]) = op match { case Std.Error.Raise(e) => pure(Left(e)) }
      })
  }

  /** `shift`: the rest of the program up to the nearest `reset` taken as `k`, a function — resumable any number of
   * times, each resumption under the same `reset` — and `f` run in its place, its value the `reset`'s. The answer
   * type `B` is the `reset`'s, fixed */
  def shift[A, B, R[_], Rr[_]](f: (A => B ! R) => B ! R)(implicit in: In[({ type L[x] = Control[B ! R, x] })#L, Rr]): A ! Rr =
    perform[({ type L[x] = Control[B ! R, x] })#L].apply[Rr, A](Std.Control.Shift[B ! R, A](f))
  /** `reset`: the delimiter `shift` takes the rest up to — the handler of `Control`, deep, answering `Shift(f)` with
   * `f(k)` */
  def reset[B, R[_]](p: B ! ({ type L[x] = Sum[R, ({ type M[y] = Control[B ! R, y] })#M, x] })#L): B ! R =
    handleLast[({ type L[x] = Control[B ! R, x] })#L, R, B, B](p, new Handler[({ type L[x] = Control[B ! R, x] })#L, R, B, B] {
      def ret(b: B) = pure(b)
      def op[X](op: Control[B ! R, X], k: X => B ! R) = op match { case Std.Control.Shift(f) => f(k) }
    })

  /** the value, or `h` given what was raised: the rest of the program dropped, `h`'s program in its place — in the
   * row without this `Error`, so one raised again goes to the next handler of it */
  def recover[E]: RecoverHandler[E] = new RecoverHandler[E]
  final class RecoverHandler[E] {
    def apply[R[_], Rest[_], A](p: A ! R)(h: E => A ! Rest)(implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): A ! Rest =
      attempt[E](p).flatMap {
        case Right(a) => pure[A, Rest](a)
        case Left(e) => h(e)
      }
  }

  /** a program whose one effect is `Lift[G, *]` as a `G[A]`: each lifted `G[X]` bound in `G`, the rest run inside
   * `G`'s bind — a program of effects run in `IO`, a ZIO task, an `Option` */
  def lower[G[_], A](p: A ! ({ type L[x] = Lift[G, x] })#L)(implicit G: Lowering[G]): G[A] = {
    type L[x] = Lift[G, x]
    run(handle[L, L, Pure, A, G[A]](p, new Handler[L, Pure, A, G[A]] {
      def ret(a: A) = pure[G[A], Pure](G.pure(a))
      def op[X](op: Lift[G, X], k: X => G[A] ! Pure) = op match {
        case Std.Lift.Of(ga) => pure[G[A], Pure](G.flatMap(ga)(x => run(k(x))))
      }
    }))
  }

  /** what the generator gave out, in order, and its value. Answered in place */
  def collect[O]: CollectHandler[O] = new CollectHandler[O]
  final class CollectHandler[O] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], A) ! Rest =
      handle[({ type L[x] = Yield[O, x] })#L, List[O], R, Rest, A, (List[O], A)](p, Nil,
        new StatefulTailHandler[({ type L[x] = Yield[O, x] })#L, List[O], Rest, A, (List[O], A)] {
          def ret(os: List[O], a: A) = pure((os.reverse, a))
          def answer[X](cell: Cell[List[O]], op: Yield[O, X]): X = op match {
            case Std.Yield.Emit(o, answer) => cell.set(o :: cell.get); answer(())
          }
        })
  }

  /** each value the generator gives out to `f`, a program of the row outside — the generator going on after it */
  def each[O]: EachHandler[O] = new EachHandler[O]
  final class EachHandler[O] {
    def apply[R[_], Rest[_], A](p: A ! R)(f: O => Unit ! Rest)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = Yield[O, x] })#L, R, Rest, A, A](p, new Handler[({ type L[x] = Yield[O, x] })#L, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: Yield[O, X], k: X => A ! Rest) = op match {
          case Std.Yield.Emit(o, answer) => f(o).flatMap(_ => k(answer(())))
        }
      })
  }

  /** the first `n` values given out, and the generator's value if it ended by then: stopped at the next one */
  def take[O](n: Int): TakeHandler[O] = new TakeHandler[O](n)
  final class TakeHandler[O](n: Int) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], Option[A]) ! Rest =
      handle[({ type L[x] = Yield[O, x] })#L, (Int, List[O]), R, Rest, A, (List[O], Option[A])](p, (n, Nil),
        new StatefulHandler[({ type L[x] = Yield[O, x] })#L, (Int, List[O]), Rest, A, (List[O], Option[A])] {
          def ret(s: (Int, List[O]), a: A) = pure((s._2.reverse, Some(a)))
          def op[X](s: (Int, List[O]), op: Yield[O, X], k: (X, (Int, List[O])) => (List[O], Option[A]) ! Rest) =
            op match {
              case Std.Yield.Emit(o, answer) =>
                if (s._1 <= 0) pure[(List[O], Option[A]), Rest]((s._2.reverse, None)) else k(answer(()), (s._1 - 1, o :: s._2))
            }
        })
  }

  /** the first way the program goes with no `fail`: the choices tried in order, the rest not tried */
  def first[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): Option[A] ! Rest =
    handle[Choose, R, Rest, A, Option[A]](p, new Handler[Choose, Rest, A, Option[A]] {
      def ret(a: A) = pure(Some(a))
      def op[X](op: Choose[X], k: X => Option[A] ! Rest) = op match {
        case Std.Choose.Among(xs) =>
          def go(ys: List[X]): Option[A] ! Rest = ys match {
            case Nil => pure[Option[A], Rest](None)
            case y :: t => k(y).flatMap {
              case None => go(t)
              case found => pure[Option[A], Rest](found)
            }
          }
          go(xs)
      }
    })

  /** every way the program can go: `k` resumed once for each choice */
  def all[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest =
    handle[Choose, R, Rest, A, List[A]](p, new Handler[Choose, Rest, A, List[A]] {
      def ret(a: A) = pure(List(a))
      def op[X](op: Choose[X], k: X => List[A] ! Rest) = op match {
        // the ways so far kept reversed, each branch's put before them: linear in the ways, not quadratic
        case Std.Choose.Among(xs) =>
          xs.foldLeft(pure[List[A], Rest](Nil))((acc, x) => acc.flatMap(l => k(x).map(r => r reverse_::: l))).map(_.reverse)
      }
    })
}
