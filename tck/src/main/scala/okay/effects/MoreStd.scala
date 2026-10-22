package okay.effects

/** the generators, the first way, recovery: one source for both compilers, rows by aliases */
class MoreStd(val fx: Runner) {
  import fx._
  import fx.std._

  type Y[X] = Yield[Int, X]
  type SI[X] = State[Int, X]
  type SY[X] = Sum[SI, Y, X]
  type Er[X] = Error[String, X]

  def nums[R[_]](n: Int)(implicit in: In[Y, R]): Unit ! R =
    if (n == 0) pure[Unit, R](()) else emit[Int, R](n).flatMap(_ => nums[R](n - 1))

  def collected: (List[Int], Unit) = run(collect[Int](nums[Y](3)))
  def taken: (List[Int], Option[Unit]) = run(take[Int](2)(nums[Y](5)))
  def all3: (List[Int], Option[Unit]) = run(take[Int](5)(nums[Y](3)))
  /** each value to a consumer of its own effect, a state outside */
  def summed: (Int, Unit) = run(state(0)(each[Int](nums[SY](3))(o => modify[Int, SI](_ + o))))

  def evens(xs: List[Int]): Option[Int] =
    run(first(among[Int, Choose](xs).flatMap(x => if (x % 2 == 0) pure[Int, Choose](x * 10) else fail[Int, Choose])))
  def many(n: Int): Int = run(all(among[Int, Choose]((1 to n).toList))).length

  // ---- the cut
  def two: Int ! Choose = among[Int, Choose](List(1, 2))
  /** `once`: the first way only, under `all` too */
  def onceAll: List[Int] = run(all(once[Int, Choose](among[Int, Choose](List(1, 2, 3)))))
  /** a cut drops only the alternatives made since its mark: the outer choice stays */
  def cutInner: List[Int] = run(all(two.flatMap(a => once[Int, Choose](among[Int, Choose](List(10, 20))).map(a + _))))
  /** a failure after a cut does not come back into what was cut */
  def failAfterCut: Option[Int] =
    run(first(once[Int, Choose](two).flatMap(x => if (x == 1) fail[Int, Choose] else pure[Int, Choose](x))))
  /** the same without the cut: back to the second way */
  def failNoCut: Option[Int] =
    run(first(two.flatMap(x => if (x == 1) fail[Int, Choose] else pure[Int, Choose](x))))
  /** a cut back to a mark taken before the choice: the choice's other ways gone, the earlier ones kept */
  def markCut: List[Int] = run(all(two.flatMap(a =>
    mark[Choose].flatMap(m => among[Int, Choose](List(10, 20)).flatMap(b => cut[Choose](m).map(_ => a + b))))))
  /** `n` choices each cut as taken: what is kept behind does not grow — a million under `first` */
  def chainCut(n: Int): Option[Int] = {
    def loop(i: Int, acc: Int): Int ! Choose =
      if (i == 0) pure[Int, Choose](acc) else once[Int, Choose](two).flatMap(x => loop(i - 1, acc + x))
    run(first(loop(n, 0)))
  }

  /** a program of Option's lifted, lowered to an Option by a `Lowering` of one's own */
  implicit val optionLowering: Std.Lowering[Option] = new Std.Lowering[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](ga: Option[A])(f: A => Option[B]): Option[B] = ga.flatMap(f)
  }
  type LO[X] = Lift[Option, X]
  def lowered(b: Option[Int]): Option[Int] =
    lower[Option, Int](lift[Option, Int, LO](Some(1)).flatMap(a => lift[Option, Int, LO](b).map(_ + a)))

  def recovered: Int = run(recover[String](raise[String, Er, Int]("boom"))(e => pure[Int, Pure](e.length)))
  def unraised: Int = run(recover[String](pure[Int, Er](7))(e => pure[Int, Pure](0)))
}
