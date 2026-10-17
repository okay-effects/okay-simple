package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState}
import okay.effects.*
import okay.effects.simple.Machine
import okay.effects.core.Core
import okay.effects.kernel.OnKernel

/** the same programs on each implementation, n = 1000: binds alone; `ask` under its handler; `ask` across four
 * handlers of other effects; a state's get and put */
class Lanes(val fx: Effects) {
  import fx.*
  import fx.std.*
  val n = 1000

  def binds: Int = run((1 to n).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1)))

  type R0 = Reader % Int
  def asks0(i: Int): Int ! R0 = if (i == 0) pure(0) else ask[Int, R0].flatMap(x => asks0(i - 1).map(_ + x))
  def askNear: Int = run(reader(1)(asks0(n)))

  type R4 = Reader % Int + State % Int + Writer % Int + Error % Int + Choose
  def asks4(i: Int): Int ! R4 = if (i == 0) pure(0) else ask[Int, R4].flatMap(x => asks4(i - 1).map(_ + x))
  def askAcross4: Int = run(reader(1)(state(0)(writer[Int](attempt[Int](all(asks4(n))))))) match {
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)
  }

  type RS = State % Int
  def counting(i: Int): Unit ! RS = if (i == 0) pure(()) else get[Int, RS].flatMap(s => put[Int, RS](s + 1)).andThen(counting(i - 1))
  def stateLoop: Int = run(state(0)(counting(n)))._1

  // -- THE SAME, ROW-POLYMORPHIC: the effects asked as evidence, `In`, once per call, passed on to the recursion —
  // the row named once, outside; its places made once, not at each operation

  def asksIn[R[_]](i: Int)(implicit in: In[Reader % Int, R]): Int ! R =
    if (i == 0) pure(0) else ask[Int, R].flatMap(x => asksIn[R](i - 1).map(_ + x))
  def askNearIn: Int = run(reader(1)(asksIn[R0](n)))
  def askAcross4In: Int = run(reader(1)(state(0)(writer[Int](attempt[Int](all(asksIn[R4](n))))))) match {
    case (_, (_, Right(List(v)))) => v
    case other => sys.error(other.toString)
  }
  def countingIn[R[_]](i: Int)(implicit in: In[State % Int, R]): Unit ! R =
    if (i == 0) pure(()) else get[Int, R].flatMap(s => put[Int, R](s + 1)).andThen(countingIn[R](i - 1))
  def stateLoopIn: Int = run(state(0)(countingIn[RS](n)))._1
}

@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ImplementationsBenchmark {
  @Param(Array("machine", "free", "okay-core", "kernel-steps", "kernel-java", "kernel-okay-core"))
  var impl: String = ""
  var lanes: Lanes = null

  @Setup def setup(): Unit = lanes = new Lanes(impl match {
    case "machine" => Machine
    case "free" => okay.effects.simple.free.Free
    case "okay-core" => Core
    case "kernel-steps" => OnKernel.on(okay.kernel.steps.Steps)
    case "kernel-java" => OnKernel.on(okay.core.Kernel.EFFECTS)
    case _ => OnKernel.on(okay.core.kernel.OnCore)
  })

  @Benchmark def binds: Int = lanes.binds
  @Benchmark def askNear: Int = lanes.askNear
  @Benchmark def askAcross4: Int = lanes.askAcross4
  @Benchmark def stateLoop: Int = lanes.stateLoop
  @Benchmark def askNearIn: Int = lanes.askNearIn
  @Benchmark def askAcross4In: Int = lanes.askAcross4In
  @Benchmark def stateLoopIn: Int = lanes.stateLoopIn
}
