package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.tagless.Tagless.*
import okay.effects.tagless.Tagless.std.*

/** `Lanes`' programs written over tagless: each made, then run on the implementation given */
class TaglessLanes(using val fx: Runner) {
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
}

@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class TaglessBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  var lanes: TaglessLanes = null

  @Setup def setup(): Unit = {
    val fx: Runner = if (impl == "machine") Machine else okay.effects.free.Free
    lanes = new TaglessLanes(using fx)
  }

  @Benchmark def binds: Int = lanes.binds
  @Benchmark def askNear: Int = lanes.askNear
  @Benchmark def askAcross4: Int = lanes.askAcross4
  @Benchmark def stateLoop: Int = lanes.stateLoop
}
