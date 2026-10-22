package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{
  Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState
}
import okay.effects.Runner

/** THE CHOICE'S HANDLERS: `first` searching by backtracking, `all` gathering every way, on the machine and on `Free`:
 * `sbt "bench/Jmh/run -f 1 -wi 3 -i 5 ChoiceBenchmark"`.
 *  - `firstSearch`: 16 two-way choices, only the last way of all accepted — 65 536 ways tried, back and back;
 *  - `allWays`: every way of 12 two-way choices, 4096 values;
 *  - `firstChain`: 100 000 one-way choices, a chain in tail position.
 * The history is in bench.d/ */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ChoiceBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""

  var search: () => Option[Int] = null
  var ways: () => Int = null
  var chain: () => Option[Int] = null

  @Setup def setup(): Unit = {
    val fx: Runner = if (impl == "machine") okay.effects.machine.Machine else okay.effects.free.Free
    import fx._
    import fx.std._
    def bits(n: Int, acc: Int): Int ! Choose =
      if (n == 0) pure[Int, Choose](acc)
      else among[Int, Choose](List(0, 1)).flatMap(b => bits(n - 1, acc * 2 + b))
    def ones(n: Int): Int ! Choose = bits(n, 0).flatMap(v => if (v == (1 << n) - 1) pure[Int, Choose](v)
      else fail[Int, Choose])
    def single(n: Int, acc: Int): Int ! Choose =
      if (n == 0) pure[Int, Choose](acc) else among[Int, Choose](List(1)).flatMap(x => single(n - 1, acc + x))
    search = () => run(first(ones(16)))
    ways = () => run(all(bits(12, 0))).length
    chain = () => run(first(single(100000, 0)))
  }

  @Benchmark def firstSearch: Option[Int] = search()
  @Benchmark def allWays: Int = ways()
  @Benchmark def firstChain: Option[Int] = chain()
}
