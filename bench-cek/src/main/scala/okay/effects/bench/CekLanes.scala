package okay.effects.bench

import okay.scala3.{!, %, +, Has, Pure, pure, run, flatMap, map, andThen}
import okay.scala3.std.{Choose, Fail, Reader, State, Writer}

/** the lanes of the benchmark, each a run: okay-simple's `Lanes` on an implementation, or `CekLanes` */
trait Lane:
  def binds: Int
  def askNear: Int
  def askAcross4: Int
  def stateLoop: Int
  def askNearIn: Int
  def askAcross4In: Int
  def stateLoopIn: Int

/** `Lanes`' programs in okay-scala's own words, on its machine */
class CekLanes extends Lane:
  val n = 1000

  def binds: Int = run((1 to n).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1)))

  type R0[X] = (Reader % Int)[X]
  def asks0(i: Int): Int ! R0 = if i == 0 then pure(0) else Reader.ask[Int, R0].flatMap(x => asks0(i - 1).map(_ + x))
  def askNear: Int = run(Reader.run(1)(asks0(n)))

  type R4[X] = (Reader % Int + State % Int + Writer % Int + Fail % Int + Choose)[X]
  def asks4(i: Int): Int ! R4 = if i == 0 then pure(0) else Reader.ask[Int, R4].flatMap(x => asks4(i - 1).map(_ + x))
  def askAcross4: Int = run(Reader.run(1)(State.run(0)(Writer.run(Fail.run(Choose.all(asks4(n))))))) match
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)

  type RS[X] = (State % Int)[X]
  def counting(i: Int): Unit ! RS =
    if i == 0 then pure(()) else State.get[Int, RS].flatMap(s => State.put[Int, RS](s + 1)).andThen(counting(i - 1))
  def stateLoop: Int = run(State.run(0)(counting(n)))._1

  // the same, row-polymorphic: the evidence asked once per call, passed on to the recursion

  def asksIn[R[_]: Has[Reader % Int]](i: Int): Int ! R =
    if i == 0 then pure(0) else Reader.ask.flatMap(x => asksIn[R](i - 1).map(_ + x))
  def askNearIn: Int = run(Reader.run(1)(asksIn[R0](n)))
  def askAcross4In: Int = run(Reader.run(1)(State.run(0)(Writer.run(Fail.run(Choose.all(asksIn[R4](n))))))) match
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)
  def countingIn[R[_]: Has[State % Int]](i: Int): Unit ! R =
    if i == 0 then pure(()) else State.get.flatMap(s => State.put(s + 1)).andThen(countingIn[R](i - 1))
  def stateLoopIn: Int = run(State.run(0)(countingIn[RS](n)))._1
