package okay.effects.bench

import okay.effects._

/** the same programs on each implementation, n = 1000, Scala 2's rows by names: binds alone; `ask` under its
 * handler; `ask` across four handlers of other effects; a state's get and put */
class Lanes(val fx: Runner) {
  import fx._
  import fx.std._
  val n = 1000

  def binds: Int = run((1 to n).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1)))

  type R0[X] = (Reader % Int)#L[X]
  def asks0(i: Int): Int ! R0 = if (i == 0) pure[Int, R0](0) else ask[Int, R0].flatMap(x => asks0(i - 1).map(_ + x))
  def askNear: Int = run(reader(1)(asks0(n)))

  type R4[X] = ((Reader % Int) + (State % Int) + (Writer % Int) + (Error % Int) + *[Choose])#L[X]
  def asks4(i: Int): Int ! R4 = if (i == 0) pure[Int, R4](0) else ask[Int, R4].flatMap(x => asks4(i - 1).map(_ + x))
  def askAcross4: Int = run(reader(1)(state(0)(writer[Int](attempt[Int](all(asks4(n))))))) match {
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)
  }

  type RS[X] = (State % Int)#L[X]
  def counting(i: Int): Unit ! RS =
    if (i == 0) pure[Unit, RS](()) else get[Int, RS].flatMap(s => put[Int, RS](s + 1)).andThen(counting(i - 1))
  def stateLoop: Int = run(state(0)(counting(n)))._1

  def asksIn[R[_]](i: Int)(implicit in: In[(Reader % Int)#L, R]): Int ! R =
    if (i == 0) pure[Int, R](0) else ask[Int, R].flatMap(x => asksIn[R](i - 1).map(_ + x))
  def askNearIn: Int = run(reader(1)(asksIn[R0](n)))
  def askAcross4In: Int = run(reader(1)(state(0)(writer[Int](attempt[Int](all(asksIn[R4](n))))))) match {
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)
  }
  def countingIn[R[_]](i: Int)(implicit in: In[(State % Int)#L, R]): Unit ! R =
    if (i == 0) pure[Unit, R](()) else get[Int, R].flatMap(s => put[Int, R](s + 1)).andThen(countingIn[R](i - 1))
  def stateLoopIn: Int = run(state(0)(countingIn[RS](n)))._1
}
