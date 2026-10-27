package okay.effects

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** other monads in a program: data monads handled in place (`handleLift`, a `Distributes`), one control monad lowered
 * last (`lowerLast`), several brought to one (`liftVia`) — layered monads. One source for both compilers, rows by
 * aliases; futures completed at once, on a parasitic context, so their values are read with no wait */
class Layered(val fx: Runner) {
  import fx._
  import fx.std._

  implicit val parasitic: ExecutionContext = ExecutionContext.parasitic
  implicit val futureLowering: Std.Lowering[Future] = new Std.Lowering[Future] {
    def pure[A](a: A): Future[A] = Future.successful(a)
    def flatMap[A, B](ga: Future[A])(f: A => Future[B]): Future[B] = ga.flatMap(f)
  }
  implicit val eitherLowering: Std.Lowering[E] = new Std.Lowering[E] {
    def pure[A](a: A): E[A] = Right(a)
    def flatMap[A, B](ga: E[A])(f: A => E[B]): E[B] = ga.flatMap(f)
  }
  def now[A](f: Future[A]): A = f.value.get.get

  type E[A] = Either[String, A]
  type LO[X] = Lift[Option, X]
  type LL[X] = Lift[List, X]
  type LV[X] = Lift[Vector, X]
  type LF[X] = Lift[Future, X]
  type LT[X] = Lift[Try, X]
  type LE[X] = Lift[E, X]

  // ---- the order of handling

  type OL[X] = Sum[LO, LL, X]
  type Row[X] = Sum[OL, LF, X]

  /** each of 1, 2, 3 through a future, then an `Option`: none at `stop` */
  def three(stop: Int): Int ! Row =
    lift[List, Int, Row](List(1, 2, 3)).flatMap(x =>
      lift[Future, Int, Row](Future.successful(x * 10)).flatMap(y =>
        lift[Option, Int, Row](if (x == stop) None else Some(y + 1))))

  def optionFirst(stop: Int): List[Option[Int]] = {
    val f: Future[List[Option[Int]]] = lowerLast[Future, ({ type L[x] = Sum[Pure, LF, x] })#L, List[Option[Int]]](
      handleLift[List](handleLift[Option](three(stop))))
    now(f)
  }
  def listFirst(stop: Int): Option[List[Int]] = {
    val f: Future[Option[List[Int]]] = lowerLast[Future, ({ type L[x] = Sum[Pure, LF, x] })#L, Option[List[Int]]](
      handleLift[Option](handleLift[List](three(stop))))
    now(f)
  }

  type TV[X] = Sum[LT, LV, X]
  type TVE[X] = Sum[TV, LE, X]
  /** three data monads, no control monad: all handled in place, run */
  def dataOnly: E[Vector[Option[Int]]] = {
    val p: Int ! TVE =
      lift[Vector, Int, TVE](Vector(1, 2)).flatMap(a =>
        lift[E, Int, TVE](if (a > 5) Left("big") else Right(a * 2)).flatMap(b =>
          lift[Try, Int, TVE](if (b == 4) Failure(new IllegalStateException("four")) else Success(b + 1))))
    run(handleLift[E](handleLift[Vector](handleLift[Try](p)))).map(_.map(_.toOption))
  }

  // ---- liftVia

  val toEither: Std.~>[Option, E] = new Std.~>[Option, E] { def apply[X](o: Option[X]): E[X] = o.toRight("none") }
  val toFuture: Std.~>[Try, Future] = new Std.~>[Try, Future] { def apply[X](t: Try[X]): Future[X] = Future.fromTry(t) }

  type OE[X] = Sum[LO, LE, X]
  def optionToEither(o: Option[Int]): E[Int] = {
    val p: Int ! OE = lift[E, Int, OE](Right(1)).flatMap(a => lift[Option, Int, OE](o).map(_ + a))
    lowerLast[E, ({ type L[x] = Sum[Pure, LE, x] })#L, Int](liftVia[Option, E](toEither)(p))
  }
  type TF[X] = Sum[LT, LF, X]
  def tryToFuture(t: Try[Int]): Try[Int] = {
    val p: Int ! TF = lift[Future, Int, TF](Future.successful(1)).flatMap(a => lift[Try, Int, TF](t).map(_ + a))
    lowerLast[Future, ({ type L[x] = Sum[Pure, LF, x] })#L, Int](liftVia[Try, Future](toFuture)(p)).value.get
  }

  // ---- the laws of flatMapIn, effectful continuations, as properties over seeded cases

  type SI[X] = State[Int, X]
  type WS[X] = Writer[String, X]
  type LR[X] = Sum[SI, WS, X]

  def ran[A](p: A ! LR): (List[String], (Int, A)) = run(writer[String](state(1)(p)))

  /** an effectful continuation of effects that DO NOT commute: it reads the state, writes it, tells */
  def noncommuting[G[_]](tag: String, make: (Int, Int) => G[Int]): Int => G[Int] ! LR =
    a => get[Int, LR].flatMap(s => put[Int, LR](s * 31 + a).flatMap(_ =>
      tell[String, LR](tag + a + "@" + s).map(_ => make(a, s))))
  /** an effectful continuation of effects that commute: a sum, not read */
  def commuting[G[_]](make: (Int, Int) => G[Int]): Int => G[Int] ! LR =
    a => modify[Int, LR](_ + a).map(_ => make(a, 0))

  /** the first law that fails in 300 seeded cases, or none */
  def laws[G[_]](gen: scala.util.Random => G[Int], make: (Int, Int) => G[Int], commute: Boolean)(
    implicit G: Std.Distributes[G]): Option[String] = {
    val random = new scala.util.Random(7)
    (0 until 300).iterator.map { i =>
      val a = random.nextInt(100) - 50
      val ga = gen(random)
      val k = noncommuting[G]("k" + i + ".", make)
      val h = noncommuting[G]("h" + i + ".", (x, s) => make(x * 7 + 3, s + i))
      val left = ran(G.flatMapIn[Int, Int, LR](fx)(G.pure(a))(k)) == ran(k(a))
      val right = ran(G.flatMapIn[Int, Int, LR](fx)(ga)(x => pure[G[Int], LR](G.pure(x)))) == ran(pure[G[Int], LR](ga))
      val (k2, h2) =
        if (commute) (commuting[G](make), commuting[G]((x, s) => make(x * 7 + 3, s + i))) else (k, h)
      val assoc = ran(G.flatMapIn[Int, Int, LR](fx)(ga)(k2).flatMap(gb => G.flatMapIn[Int, Int, LR](fx)(gb)(h2))) ==
        ran(G.flatMapIn[Int, Int, LR](fx)(ga)(x => k2(x).flatMap(gb => G.flatMapIn[Int, Int, LR](fx)(gb)(h2))))
      if (!left) Some(s"left identity at $a") else if (!right) Some(s"right identity at $ga")
      else if (!assoc) Some(s"associativity at $ga") else None
    }.collectFirst { case Some(failed) => failed }
  }

  def optionLaws: Option[String] = laws[Option](r => if (r.nextInt(4) > 0) Some(r.nextInt(100)) else None,
    (a, s) => if ((a + s) % 3 != 0) Some(a + s) else None, commute = false)
  def eitherLaws: Option[String] =
    laws[E](r => if (r.nextInt(4) > 0) Right(r.nextInt(100)) else Left("e" + r.nextInt(9)),
    (a, s) => if ((a + s) % 3 != 0) Right(a - s) else Left("at " + a), commute = false)(Std.Distributes.either[String])
  def tryLaws: Option[String] =
    laws[Try](r => if (r.nextInt(4) > 0) Success(r.nextInt(100)) else Failure(Oops(r.nextInt(9))),
    (a, s) => if ((a + s) % 3 != 0) Success(a * 2 + s) else Failure(Oops(a)), commute = false)
  def listLaws: Option[String] = laws[List](r => List.fill(r.nextInt(5))(r.nextInt(100)),
    (a, s) => List.tabulate(math.abs(a + s) % 4)(j => a + s + j), commute = true)
  def vectorLaws: Option[String] = laws[Vector](r => Vector.fill(r.nextInt(5))(r.nextInt(100)),
    (a, s) => Vector.tabulate(math.abs(a + s) % 4)(j => a - s + j), commute = true)

  /** List's associativity under effects that do not commute: the tags told, breadth first and depth first; and
   * `handleLift`'s, depth first */
  def breadthDepthHandled: (List[String], List[String], List[String]) = {
    val G = Std.Distributes.list
    val k = noncommuting[List]("k", (a, _) => List(a, a + 1))
    val h = noncommuting[List]("h", (b, _) => List(b))
    def tags(told: List[String]) = told.map(_.takeWhile(_.isLetter))
    val breadth = ran(G.flatMapIn[Int, Int, LR](fx)(List(1, 2))(k).flatMap(G.flatMapIn[Int, Int, LR](fx)(_)(h)))._1
    val depth =
      ran(G.flatMapIn[Int, Int, LR](fx)(List(1, 2))(x => k(x).flatMap(G.flatMapIn[Int, Int, LR](fx)(_)(h))))._1
    type R[X] = Sum[LR, LL, X]
    val p: Int ! R =
      lift[List, Int, R](List(1, 2)).flatMap(x => tell[String, R]("k").flatMap(_ =>
        lift[List, Int, R](List(x, x + 1)).flatMap(b => tell[String, R]("h").map(_ => b))))
    (tags(breadth), tags(depth), run(writer[String](state(0)(handleLift[List](p))))._1)
  }

  // ---- with State

  type SL[X] = Sum[SI, LL, X]
  /** each element: the state read, the element added */
  def adding: (Int, Int) ! SL =
    lift[List, Int, SL](List(1, 2, 3)).flatMap(x =>
      get[Int, SL].flatMap(s => put[Int, SL](s + x).map(_ => (x, s))))
  def stateOutside: (Int, List[(Int, Int)]) = run(state(0)(handleLift[List](adding)))
  def stateInside: List[(Int, (Int, Int))] = run(handleLift[List](state(0)(adding)))

  // ---- constant stack

  type SLL[X] = Sum[({ type L[x] = State[Long, x] })#L, LL, X]
  /** a List of 10 000 elements, a state through them: the sum, the length, the last */
  def longList: (Long, Int, Int) = {
    val p: Int ! SLL = lift[List, Int, SLL](List.range(0, 10000)).flatMap(x => modify[Long, SLL](_ + x).map(_ => x))
    val (sum, xs) = run(state(0L)(handleLift[List](p)))
    (sum, xs.length, xs.last)
  }
  def longVector: (Int, Int) = {
    val v = run(handleLift[Vector](lift[Vector, Int, LV](Vector.range(0, 10000)).map(_ * 2)))
    (v.length, v.last)
  }
  /** `n` lifts in a chain, each of its own monad's: of Option, of a one-element List, through liftVia */
  def chains(n: Int): (Option[Int], List[Int], E[Int]) = {
    def chain[R[_]](one: Int => Int ! R): Int ! R =
      tailRecM[Int, Int, R](0)(i => one(i).map(j => if (j == n) Right(j) else Left(j + 1)))
    val o = run(handleLift[Option](chain[LO](i => lift[Option, Int, LO](Some(i)))))
    val l = run(handleLift[List](chain[LL](i => lift[List, Int, LL](List(i)))))
    // handled in place: `lower` of a strict monad nests its runs, one per operation, so it takes stack
    val e = run(handleLift[E](liftVia[Option, E](toEither)(chain[OE](i => lift[Option, Int, OE](Some(i))))))
    (o, l, e)
  }
}

/** a failure equal by its number, for comparing `Try`s */
final case class Oops(n: Int) extends Exception("oops " + n)
