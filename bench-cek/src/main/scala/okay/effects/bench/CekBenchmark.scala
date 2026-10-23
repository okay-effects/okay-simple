package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State}
import okay.effects.Runner

/** OKAY-SCALA'S MACHINE BESIDE THIS ONE: `Lanes`' programs on the machine, the same on okay-scala's (`OnCek`), and
 * okay-scala's own words on its machine. `sbt "bench/Jmh/run -f 2 -wi 5 -i 5 CekBenchmark"` */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CekBenchmark:
  @Param(Array("machine", "onCek", "cek"))
  var impl: String = ""
  var lane: Lane = null

  @Setup def setup(): Unit = lane = impl match
    case "machine" => OnRunner(okay.effects.machine.Machine)
    case "onCek" => OnRunner(okay.effects.cek.OnCek)
    case "cek" => CekLanes()

  @Benchmark def binds: Int = lane.binds
  @Benchmark def askNear: Int = lane.askNear
  @Benchmark def askAcross4: Int = lane.askAcross4
  @Benchmark def stateLoop: Int = lane.stateLoop
  @Benchmark def askNearIn: Int = lane.askNearIn
  @Benchmark def askAcross4In: Int = lane.askAcross4In
  @Benchmark def stateLoopIn: Int = lane.stateLoopIn

/** `Lanes` on an implementation, as a `Lane` */
final class OnRunner(fx: Runner) extends Lane:
  private val lanes = Lanes(fx)
  def binds: Int = lanes.binds
  def askNear: Int = lanes.askNear
  def askAcross4: Int = lanes.askAcross4
  def stateLoop: Int = lanes.stateLoop
  def askNearIn: Int = lanes.askNearIn
  def askAcross4In: Int = lanes.askAcross4In
  def stateLoopIn: Int = lanes.stateLoopIn
