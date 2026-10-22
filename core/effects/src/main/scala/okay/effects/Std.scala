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

  /** an effect of no parameter: `Choose` in a Scala 3 row, `*[Choose]#L` in a Scala 2 one. `Among` the choice;
   * `Mark` and `Cut` a CUT, Prolog's: `Mark` says how many alternatives are pending, `Cut(m)` drops those made since
   * — the ways not yet tried. `first` and `all` drop them; a fair search, which has no order to cut in, keeps them */
  sealed trait Choose[X]
  object Choose {
    final case class Among[X](xs: List[X]) extends Choose[X]
    /** the alternatives pending: a mark to cut back to */
    final case class Mark[X](answer: Int => X) extends Choose[X]
    /** the alternatives made since the mark dropped */
    final case class Cut[X](mark: Int, answer: Unit => X) extends Choose[X]
  }

  /** A PROGRAM OF ANOTHER MONAD'S, as an effect: `Of(ga)`, a `G[X]` performed as it is, its value the answer — `IO`,
   * a ZIO task, an `Option`, a `Future`, anything `Runner.lower` can bind (`Lowering`) */
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

  /** an early end with no value: `Exit()`, the rest dropped — `optional` gives `None` */
  sealed trait Abort[X]
  object Abort {
    final case class Exit[X]() extends Abort[X]
  }

  /** a supply of fresh numbers: `Next`, each one never given before in the run */
  sealed trait Fresh[X]
  object Fresh {
    final case class Next[X](answer: Long => X) extends Fresh[X]
  }

  /** random numbers, from a seed: `NextLong`, the next of a generator kept by the handler — the same seed, the same
   * numbers; a `k` resumed twice resumes the generator of its time */
  sealed trait Random[X]
  object Random {
    final case class NextLong[X](answer: Long => X) extends Random[X]
  }

  /** A STORE OF VALUES `V` BY KEY `K`: in a row `({ type L[x] = Store[K, V, x] })#L`, Scala 3's `[x] =>> Store[K, V,
   * x]` */
  sealed trait Store[K, V, X]
  object Store {
    final case class Load[K, V, X](k: K, answer: Option[V] => X) extends Store[K, V, X]
    final case class Save[K, V, X](k: K, v: V, answer: Unit => X) extends Store[K, V, X]
    final case class Remove[K, V, X](k: K, answer: Unit => X) extends Store[K, V, X]
  }

  /** splitmix64's step: the next state and its number */
  private[effects] def splitmix(s: Long): (Long, Long) = {
    val n = s + 0x9e3779b97f4a7c15L
    var z = n
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    (n, z ^ (z >>> 31))
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
 * `import Machine._; import Machine.std._`. The effects (`State`, `Reader`, `Writer`, `Error`, `Choose`, `Control` for
 * shift and reset), their
 * operations and their handlers, each handler taking its effect from anywhere in the row; the parameter named, the
 * rest inferred: `writer[String](p)`. A handler answering in place is the implementation's `handle` of its shape.
 * An effect with a parameter is written here as both compilers read it, `({ type L[x] = State[S, x] })#L` — Scala 3's
 * `State % S`, Scala 2's `(State % S)#L`.
 */
class Std[F <: Effects with Singleton](val fx: F) {
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
  type Abort[X] = Std.Abort[X]
  val Abort: Std.Abort.type = Std.Abort
  type Fresh[X] = Std.Fresh[X]
  val Fresh: Std.Fresh.type = Std.Fresh
  type Random[X] = Std.Random[X]
  val Random: Std.Random.type = Std.Random
  type Store[K, V, X] = Std.Store[K, V, X]
  val Store: Std.Store.type = Std.Store

  /** THE WORDS OF ONE ROW: the row named once, the words without it — `object service extends std.On[Service]`,
   * then in it (or after `import service.*`) `ask[Config]`, `get[Int]`, `put(n)`, `tell(line)`, `raise(e)`. Each
   * word is the standard one at `R`; what is left to give is only what the row does not decide */
  class On[R[_]] {
    /** the row itself, by name: `def tick: Int ! Row` in an object extending `std.On[Reader % Int + State % Int]` */
    type Row[X] = R[X]
    def pure[A](a: A): A ! R = fx.pure[A, R](a)
    def ask[E](implicit in: In[({ type L[x] = Reader[E, x] })#L, R]): E ! R = Std.this.ask[E, R]
    def get[S](implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R = Std.this.get[S, R]
    def put[S](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = Std.this.put[S, R](s)
    def modify[S](f: S => S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R = Std.this.modify[S, R](f)
    def tell[W](w: W)(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R = Std.this.tell[W, R](w)
    /** a raise is a program of no value, so of any: `Nothing ! R` */
    def raise[E](e: E)(implicit in: In[({ type L[x] = Error[E, x] })#L, R]): Nothing ! R =
      Std.this.raise[E, R, Nothing](e)
    def among[X](xs: List[X])(implicit in: In[Choose, R]): X ! R = Std.this.among[X, R](xs)
    def fail(implicit in: In[Choose, R]): Nothing ! R = Std.this.fail[Nothing, R]
    def once[A](p: => A ! R)(implicit in: In[Choose, R]): A ! R = Std.this.once[A, R](p)
  }
  /** the words of the row `R`: `val words = std.on[Service]; import words.*` */
  def on[R[_]]: On[R] = new On[R]

  def get[S, R[_]](implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R =
    perform[({ type L[x] = State[S, x] })#L].apply[R, S](Std.State.Get(s => s))
  def put[S, R[_]](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = State[S, x] })#L].apply[R, Unit](Std.State.Put(s, u => u))
  def modify[S, R[_]](f: S => S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): Unit ! R =
    get[S, R].flatMap(s => put[S, R](f(s)))
  def ask[E, R[_]](implicit in: In[({ type L[x] = Reader[E, x] })#L, R]): E ! R =
    perform[({ type L[x] = Reader[E, x] })#L].apply[R, E](Std.Reader.Ask(e => e))
  def tell[W, R[_]](w: W)(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Writer[W, x] })#L].apply[R, Unit](Std.Writer.Tell(w, u => u))
  def raise[E, R[_], X](e: E)(implicit in: In[({ type L[x] = Error[E, x] })#L, R]): X ! R =
    perform[({ type L[x] = Error[E, x] })#L].apply[R, X](Std.Error.Raise(e))
  def among[X, R[_]](xs: List[X])(implicit in: In[Choose, R]): X ! R = perform[Choose].apply[R, X](Std.Choose.Among(xs))
  def fail[X, R[_]](implicit in: In[Choose, R]): X ! R = among[X, R](Nil)
  /** the alternatives pending, to cut back to */
  def mark[R[_]](implicit in: In[Choose, R]): Int ! R = perform[Choose].apply[R, Int](Std.Choose.Mark[Int](m => m))
  /** the alternatives made since `m` dropped: the ways not yet tried there are not tried */
  def cut[R[_]](m: Int)(implicit in: In[Choose, R]): Unit ! R =
    perform[Choose].apply[R, Unit](Std.Choose.Cut[Unit](m, u => u))
  /** `p`'s first way, its other ways dropped once it has one — Prolog's `once/1`: a choice inside `p` is not come
   * back to by a failure after it */
  def once[A, R[_]](p: => A ! R)(implicit in: In[Choose, R]): A ! R =
    mark[R].flatMap(m => p.flatMap(a => cut[R](m).map(_ => a)))
  def lift[G[_], X, R[_]](ga: G[X])(implicit in: In[({ type L[x] = Lift[G, x] })#L, R]): X ! R =
    perform[({ type L[x] = Lift[G, x] })#L].apply[R, X](Std.Lift.Of(ga))
  def emit[O, R[_]](o: O)(implicit in: In[({ type L[x] = Yield[O, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Yield[O, x] })#L].apply[R, Unit](Std.Yield.Emit(o, u => u))

  /** the state through `f` */
  def gets[S, A, R[_]](f: S => A)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): A ! R = get[S, R].map(f)
  /** the state set to `s`, the one before given */
  def getAndSet[S, R[_]](s: S)(implicit in: In[({ type L[x] = State[S, x] })#L, R]): S ! R =
    get[S, R].flatMap(old => put[S, R](s).as(old))
  /** a choice going on only where `cond` holds */
  def guard[R[_]](cond: Boolean)(implicit in: In[Choose, R]): Unit ! R = if (cond) pure[Unit, R](()) else fail[Unit, R]
  /** the program ended here, with no value */
  def abort[X, R[_]](implicit in: In[Abort, R]): X ! R = perform[Abort].apply[R, X](Std.Abort.Exit[X]())
  /** a number never given before in this run */
  def fresh[R[_]](implicit in: In[Fresh, R]): Long ! R = perform[Fresh].apply[R, Long](Std.Fresh.Next(n => n))
  def nextLong[R[_]](implicit in: In[Random, R]): Long ! R = perform[Random].apply[R, Long](Std.Random.NextLong(n => n))
  /** a number in `[0, bound)`, `bound` positive */
  def nextInt[R[_]](bound: Int)(implicit in: In[Random, R]): Int ! R =
    perform[Random].apply[R, Int](Std.Random.NextLong(n => java.lang.Math.floorMod(n, bound.toLong).toInt))
  /** a number in `[0, 1)` */
  def nextDouble[R[_]](implicit in: In[Random, R]): Double ! R =
    perform[Random].apply[R, Double](Std.Random.NextLong(n => (n >>> 11) * 1.1102230246251565e-16))
  def load[K, V, R[_]](k: K)(implicit in: In[({ type L[x] = Store[K, V, x] })#L, R]): Option[V] ! R =
    perform[({ type L[x] = Store[K, V, x] })#L].apply[R, Option[V]](Std.Store.Load[K, V, Option[V]](k, v => v))
  def save[K, V, R[_]](k: K, v: V)(implicit in: In[({ type L[x] = Store[K, V, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Store[K, V, x] })#L].apply[R, Unit](Std.Store.Save[K, V, Unit](k, v, u => u))
  def remove[K, V, R[_]](k: K)(implicit in: In[({ type L[x] = Store[K, V, x] })#L, R]): Unit ! R =
    perform[({ type L[x] = Store[K, V, x] })#L].apply[R, Unit](Std.Store.Remove[K, V, Unit](k, u => u))

  /** the final state and the value; a `k` resumed twice resumes the state of its time. Answered in place */
  def state[S](s0: S): StateHandler[S] = new StateHandler(s0)
  final class StateHandler[S](s0: S) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = State[S, x] })#L, R, Rest]): (S, A) ! Rest =
      handle[({ type L[x] = State[S, x] })#L, S, R, Rest, A, (S, A)](p, s0,
        new StatefulTailHandler[({ type L[x] = State[S, x] })#L, S, Rest, A, (S, A)] {
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
      handle[({ type L[x] = Reader[E, x] })#L, R, Rest, A, A](p,
        new TailHandler[({ type L[x] = Reader[E, x] })#L, Rest, A, A] {
          def ret(a: A) = pure(a)
          def answer[X](op: Reader[E, X]): X = op match { case Std.Reader.Ask(answer) => answer(e) }
        })
  }

  /** what was told, in order, and the value. Answered in place */
  def writer[W]: WriterHandler[W] = new WriterHandler[W]
  final class WriterHandler[W] {
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Writer[W, x] })#L, R, Rest]): (List[W], A) ! Rest =
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
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): Either[E, A] ! Rest =
      handle[({ type L[x] = Error[E, x] })#L, R, Rest, A, Either[E, A]](p,
        new AbortHandler[({ type L[x] = Error[E, x] })#L, Rest, A, Either[E, A]] {
          def ret(a: A) = pure(Right(a))
          def abort[X](op: Error[E, X]) = op match { case Std.Error.Raise(e) => pure(Left(e)) }
        })
  }

  /** the program's `Reader` answered by `f` of the environment outside: `p`'s asks see `f(e)`, the row's own `Reader`
   * stays — `p` handled by a reader of its own, its row back where it was (`Sub[Rest, R]`) */
  def local[E](f: E => E): LocalHandler[E] = new LocalHandler(f)
  final class LocalHandler[E](f: E => E) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Reader[E, x] })#L, R, Rest],
      back: Sub[Rest, R]): A ! R =
      ask[E, R].flatMap(e => reader[E](f(e))(p).into[R])
  }

  /** what `p` told, with its value — told outside too, when `p` ends */
  def listen[W]: ListenHandler[W] = new ListenHandler[W]
  final class ListenHandler[W] {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Writer[W, x] })#L, R, Rest],
      back: Sub[Rest, R]): (List[W], A) ! R =
      writer[W](p).into[R].flatMap { case (ws, a) => tellAll[W, R](ws).as((ws, a)) }
  }
  /** what `p` told, through `f`, told outside when `p` ends */
  def censor[W](f: List[W] => List[W]): CensorHandler[W] = new CensorHandler[W](f)
  final class CensorHandler[W](f: List[W] => List[W]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[({ type L[x] = Writer[W, x] })#L, R, Rest],
      back: Sub[Rest, R]): A ! R =
      writer[W](p).into[R].flatMap { case (ws, a) => tellAll[W, R](f(ws)).as(a) }
  }
  private def tellAll[W, R[_]](ws: List[W])(implicit in: In[({ type L[x] = Writer[W, x] })#L, R]): Unit ! R =
    ws.foldLeft(pure[Unit, R](()))((p, w) => p.flatMap(_ => tell[W, R](w)))

  /** an observer of `E`'s operations: a program of the row, run before each is passed on */
  trait Observe[E[_], R[_]] { def apply[X](op: E[X]): Unit ! R }
  /** each of `p`'s operations of `E` seen by `f`, then passed on as it was: an audit trail, a count, a trace */
  def observe[E[_], R[_], A](p: A ! R)(f: Observe[E, R])(implicit in: In[E, R]): A ! R =
    intercept[E, R, A, A](p,
      new Handler[E, R, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: E[X], k: X => A ! R) = f[X](op).flatMap(_ => perform[E].apply[R, X](op)).flatMap(k)
      })

  /** around each operation of `E`: given it and its way on, `proceed` (passed on as it was), the program in its place —
   * `proceed`, something before or after it, another operation, or an answer of its own and no `proceed` */
  trait Around[E[_], R[_]] { def apply[X](op: E[X], proceed: X ! R): X ! R }
  def around[E[_], R[_], A](p: A ! R)(f: Around[E, R])(implicit in: In[E, R]): A ! R =
    intercept[E, R, A, A](p,
      new Handler[E, R, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: E[X], k: X => A ! R) = f[X](op, perform[E].apply[R, X](op)).flatMap(k)
      })

  /** the value, or `None` if the program aborted: the rest dropped */
  def optional[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Abort, R, Rest]): Option[A] ! Rest =
    handle[Abort, R, Rest, A, Option[A]](p,
      new AbortHandler[Abort, Rest, A, Option[A]] {
        def ret(a: A) = pure(Some(a))
        def abort[X](op: Abort[X]) = pure(None)
      })

  /** fresh numbers from `n0` on. Answered in place */
  def freshFrom(n0: Long): FreshHandler = new FreshHandler(n0)
  final class FreshHandler(n0: Long) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Fresh, R, Rest]): A ! Rest =
      handle[Fresh, Long, R, Rest, A, A](p, n0,
        new StatefulTailHandler[Fresh, Long, Rest, A, A] {
          def ret(n: Long, a: A) = pure(a)
          def answer[X](cell: Cell[Long], op: Fresh[X]): X = op match {
            case Std.Fresh.Next(answer) => val n = cell.get; cell.set(n + 1); answer(n)
          }
        })
  }

  /** random numbers from `seed` (splitmix64). Answered in place */
  def random(seed: Long): RandomHandler = new RandomHandler(seed)
  final class RandomHandler(seed: Long) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Random, R, Rest]): A ! Rest =
      handle[Random, Long, R, Rest, A, A](p, seed,
        new StatefulTailHandler[Random, Long, Rest, A, A] {
          def ret(s: Long, a: A) = pure(a)
          def answer[X](cell: Cell[Long], op: Random[X]): X = op match {
            case Std.Random.NextLong(answer) => val (s, n) = Std.splitmix(cell.get); cell.set(s); answer(n)
          }
        })
  }

  /** the store's final map and the value, from `m0`. Answered in place */
  def store[K, V](m0: Map[K, V]): StoreHandler[K, V] = new StoreHandler(m0)
  final class StoreHandler[K, V](m0: Map[K, V]) {
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Store[K, V, x] })#L, R, Rest]): (Map[K, V], A) ! Rest =
      handle[({ type L[x] = Store[K, V, x] })#L, Map[K, V], R, Rest, A, (Map[K, V], A)](p, m0,
        new StatefulTailHandler[({ type L[x] = Store[K, V, x] })#L, Map[K, V], Rest, A, (Map[K, V], A)] {
          def ret(m: Map[K, V], a: A) = pure((m, a))
          def answer[X](cell: Cell[Map[K, V]], op: Store[K, V, X]): X = op match {
            case Std.Store.Load(k, answer) => answer(cell.get.get(k))
            case Std.Store.Save(k, v, answer) => cell.set(cell.get.updated(k, v)); answer(())
            case Std.Store.Remove(k, answer) => cell.set(cell.get - k); answer(())
          }
        })
  }

  /** `shift`: the rest of the program up to the nearest `reset` taken as `k`, a function — resumable any number of
   * times, each resumption under the same `reset` — and `f` run in its place, its value the `reset`'s. The answer
   * type `B` is the `reset`'s, fixed */
  def shift[A, B, R[_], Rr[_]](f: (A => B ! R) => B ! R)(
    implicit in: In[({ type L[x] = Control[B ! R, x] })#L, Rr]): A ! Rr =
    perform[({ type L[x] = Control[B ! R, x] })#L].apply[Rr, A](Std.Control.Shift[B ! R, A](f))
  /** `reset`: the delimiter `shift` takes the rest up to — the handler of `Control`, deep, answering `Shift(f)` with
   * `f(k)` */
  def reset[B, R[_]](p: B ! ({ type L[x] = Sum[R, ({ type M[y] = Control[B ! R, y] })#M, x] })#L): B ! R =
    handleLast[({ type L[x] = Control[B ! R, x] })#L, R, B, B](p,
      new Handler[({ type L[x] = Control[B ! R, x] })#L, R, B, B] {
        def ret(b: B) = pure(b)
        def op[X](op: Control[B ! R, X], k: X => B ! R) = op match { case Std.Control.Shift(f) => f(k) }
      })

  /** the value, or `h` given what was raised: the rest of the program dropped, `h`'s program in its place — in the
   * row without this `Error`, so one raised again goes to the next handler of it */
  def recover[E]: RecoverHandler[E] = new RecoverHandler[E]
  final class RecoverHandler[E] {
    def apply[R[_], Rest[_], A](p: A ! R)(h: E => A ! Rest)(
      implicit sp: Split[({ type L[x] = Error[E, x] })#L, R, Rest]): A ! Rest =
      attempt[E](p).flatMap {
        case Right(a) => pure[A, Rest](a)
        case Left(e) => h(e)
      }
  }

  /** what the generator gave out, in order, and its value. Answered in place */
  def collect[O]: CollectHandler[O] = new CollectHandler[O]
  final class CollectHandler[O] {
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], A) ! Rest =
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
    def apply[R[_], Rest[_], A](p: A ! R)(f: O => Unit ! Rest)(
      implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): A ! Rest =
      handle[({ type L[x] = Yield[O, x] })#L, R, Rest, A, A](p,
        new Handler[({ type L[x] = Yield[O, x] })#L, Rest, A, A] {
          def ret(a: A) = pure(a)
          def op[X](op: Yield[O, X], k: X => A ! Rest) = op match {
            case Std.Yield.Emit(o, answer) => f(o).flatMap(_ => k(answer(())))
          }
        })
  }

  /** the first `n` values given out, and the generator's value if it ended by then: stopped at the next one */
  def take[O](n: Int): TakeHandler[O] = new TakeHandler[O](n)
  final class TakeHandler[O](n: Int) {
    def apply[R[_], Rest[_], A](p: A ! R)(
      implicit sp: Split[({ type L[x] = Yield[O, x] })#L, R, Rest]): (List[O], Option[A]) ! Rest =
      handle[({ type L[x] = Yield[O, x] })#L, (Int, List[O]), R, Rest, A, (List[O], Option[A])](p, (n, Nil),
        new StatefulHandler[({ type L[x] = Yield[O, x] })#L, (Int, List[O]), Rest, A, (List[O], Option[A])] {
          def ret(s: (Int, List[O]), a: A) = pure((s._2.reverse, Some(a)))
          def op[X](s: (Int, List[O]), op: Yield[O, X], k: (X, (Int, List[O])) => (List[O], Option[A]) ! Rest) =
            op match {
              case Std.Yield.Emit(o, answer) =>
                if (s._1 <= 0) pure[(List[O], Option[A]), Rest]((s._2.reverse, None))
                else k(answer(()), (s._1 - 1, o :: s._2))
            }
        })
  }

  /** THE ALTERNATIVES NOT YET TRIED, as data: each a way on from its choice point, given the stack below it; `depth`
   * their number, the mark a cut goes back to. A choice is resumed in tail position and its other ways pushed here,
   * so nothing waits on a frame: a cut drops them and they are gone, and a chain of choice points is as long in
   * memory as the alternatives still pending */
  final class Pending[B](val stack: List[Pending[B] => B], val depth: Int) {
    def push(alt: Pending[B] => B): Pending[B] = new Pending[B](alt :: stack, depth + 1)
    def cutTo(m: Int): Pending[B] = if (m >= depth) this else new Pending[B](stack.drop(depth - m), m)
  }
  object Pending { def empty[B]: Pending[B] = new Pending[B](Nil, 0) }

  /** the first way the program goes with no `fail`: the choices tried in order, the rest not tried; a cut drops the
   * alternatives made since its mark */
  def first[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): Option[A] ! Rest = {
    type B = Option[A] ! Rest
    handle[Choose, Pending[B], R, Rest, A, Option[A]](p, Pending.empty[B],
      new StatefulHandler[Choose, Pending[B], Rest, A, Option[A]] {
        def ret(s: Pending[B], a: A) = pure(Some(a))
        /** the next alternative, or none left */
        def back(s: Pending[B]): B = s.stack match {
          case alt :: below => alt(new Pending[B](below, s.depth - 1))
          case Nil => pure[Option[A], Rest](None)
        }
        def op[X](s: Pending[B], op: Choose[X], k: (X, Pending[B]) => B) = op match {
          case Std.Choose.Among(xs) =>
            // `isEmpty`, not a `Nil` pattern: that is an `equals` call per choice point
            def go(ys: List[X], s: Pending[B]): B =
              if (ys.isEmpty) back(s)
              else if (ys.tail.isEmpty) k(ys.head, s)
              else k(ys.head, s.push(below => go(ys.tail, below)))
            go(xs, s)
          case Std.Choose.Mark(answer) => k(answer(s.depth), s)
          case Std.Choose.Cut(m, answer) => k(answer(()), s.cutTo(m))
        }
      })
  }

  /** every way the program can go: each choice's ways in turn, its others pending; a cut drops the alternatives made
   * since its mark. The ways found kept reversed: linear in the ways, not quadratic */
  def all[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Choose, R, Rest]): List[A] ! Rest = {
    type B = List[A] => List[A] ! Rest
    handle[Choose, Pending[B], R, Rest, A, List[A] => List[A] ! Rest](p, Pending.empty[B],
      new StatefulHandler[Choose, Pending[B], Rest, A, List[A] => List[A] ! Rest] {
        /** the next alternative with the ways found so far, or those ways, in order, none left */
        def back(s: Pending[B], found: List[A]): List[A] ! Rest = s.stack match {
          case alt :: below => alt(new Pending[B](below, s.depth - 1))(found)
          case Nil => pure[List[A], Rest](found.reverse)
        }
        def ret(s: Pending[B], a: A) = pure[B, Rest]((found: List[A]) => back(s, a :: found))
        def op[X](s: Pending[B], op: Choose[X], k: (X, Pending[B]) => B ! Rest) = op match {
          case Std.Choose.Among(xs) =>
            def go(ys: List[X], s: Pending[B]): B ! Rest =
              if (ys.isEmpty) pure[B, Rest]((found: List[A]) => back(s, found))
              else if (ys.tail.isEmpty) k(ys.head, s)
              else k(ys.head, s.push(below => (found: List[A]) => go(ys.tail, below).flatMap(_(found))))
            go(xs, s)
          case Std.Choose.Mark(answer) => k(answer(s.depth), s)
          case Std.Choose.Cut(m, answer) => k(answer(()), s.cutTo(m))
        }
      }).flatMap(_(Nil))
  }
}
