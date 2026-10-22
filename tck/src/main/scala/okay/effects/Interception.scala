package okay.effects

/** interceptors: `intercept`, `observe`, `around` — one source for both compilers, rows by aliases */
class Interception(val fx: Runner) {
  import fx._
  import fx.std._

  type St[X] = State[Int, X]
  type Wr[X] = Writer[String, X]
  type Rd[X] = Reader[Int, X]
  type SW[X] = Sum[({ type L[x] = Sum[Void, Wr, x] })#L, St, X]

  def counter: Int ! SW =
    put[Int, SW](1).flatMap(_ => get[Int, SW]).flatMap(n => put[Int, SW](n + 10)).flatMap(_ => get[Int, SW])

  /** the state's operations told as they go, the state working as it did */
  def observed: (List[String], (Int, Int)) =
    run(writer[String](state(0)(observe[St, SW, Int](counter)(new Observe[St, SW] {
      def apply[X](op: State[Int, X]): Unit ! SW = op match {
        case Std.State.Get(_) => tell[String, SW]("get")
        case Std.State.Put(s, _) => tell[String, SW](s"put $s")
      }
    }))))

  /** two interceptors: the inner sees an operation first, then passes it to the outer */
  def nested: (List[String], (Int, Int)) = run(writer[String](state(0)(
        observe[St, SW, Int](observe[St, SW, Int](counter)(new Observe[St, SW] {
              def apply[X](op: State[Int, X]): Unit ! SW = tell[String, SW]("inner")
            }))(new Observe[St, SW] {
            def apply[X](op: State[Int, X]): Unit ! SW = tell[String, SW]("outer")
          }))))

  /** each ask answered with the environment outside, doubled: an operation changed, another in its place */
  def doubled: Int = run(reader(21)(around[Rd, Rd, Int](ask[Int, Rd])(new Around[Rd, Rd] {
    def apply[X](op: Reader[Int, X], proceed: X ! Rd): X ! Rd = op match {
      case Std.Reader.Ask(answer) => ask[Int, Rd].map(e => answer(e * 2))
    }
  })))

  /** the choices refused but the even ones; `all` outside resuming each */
  def evensOnly: List[Int] =
    run(all(around[Choose, Choose, Int](among[Int, Choose]((1 to 6).toList))(new Around[Choose, Choose] {
      def apply[X](op: Choose[X], proceed: X ! Choose): X ! Choose = op match {
        case Std.Choose.Among(xs) => among[X, Choose](xs.filter { case n: Int => n % 2 == 0; case _ => true })
      }
    })))

  /** a general interceptor: an operation answered here, never passed on */
  def answeredHere: (Int, Int) = run(state(5)(intercept[St, St, Int, Int](get[Int, St],
        new Handler[St, St, Int, Int] {
          def ret(a: Int) = pure(a)
          def op[X](op: State[Int, X], k: X => Int ! St) = op match {
            case Std.State.Get(answer) => k(answer(100))
            case Std.State.Put(_, answer) => k(answer(()))
          }
        })))

  /** in constant stack: 100 000 operations through an interceptor */
  def many(n: Int): (Int, Int) = {
    def loop(i: Int): Unit ! St = if (i == 0) pure[Unit, St](()) else modify[Int, St](_ + 1).flatMap(_ => loop(i - 1))
    var seen = 0
    val r = run(state(0)(observe[St, St, Unit](loop(n))(new Observe[St, St] {
      def apply[X](op: State[Int, X]): Unit ! St = { seen += 1; pure[Unit, St](()) }
    })))
    (r._1, seen)
  }
}
