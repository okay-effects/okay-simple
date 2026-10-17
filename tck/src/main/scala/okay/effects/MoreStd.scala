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
