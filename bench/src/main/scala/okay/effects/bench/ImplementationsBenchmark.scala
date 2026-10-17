package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState}
import okay.effects.Runner
import okay.effects.machine.Machine

@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ImplementationsBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  var lanes: Lanes = null

  @Setup def setup(): Unit = {
    val fx: Runner = if (impl == "machine") Machine else okay.effects.free.Free
    lanes = new Lanes(fx)
  }

  @Benchmark def binds: Int = lanes.binds
  @Benchmark def askNear: Int = lanes.askNear
  @Benchmark def askAcross4: Int = lanes.askAcross4
  @Benchmark def stateLoop: Int = lanes.stateLoop
  @Benchmark def askNearIn: Int = lanes.askNearIn
  @Benchmark def askAcross4In: Int = lanes.askAcross4In
  @Benchmark def stateLoopIn: Int = lanes.stateLoopIn
}
