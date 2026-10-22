package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{
  Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState
}
import okay.effects.Runner
import okay.effects.json.{Descent, Json, Jsons}
import okay.effects.machine.Machine
import okay.effects.stream.Streams

/** THE TWO HANDLERS OF `JsonParser`: the grammar on effects against `Descent`, the recursive descent, on the machine
 * and
 * on `Free`, texts of 5 KB and 100 KB: `sbt "bench/Jmh/run -f 1 -wi 3 -i 5 JsonBenchmark"`. The ratio is the cost of
 * parsing by effects; the history is in bench.d/ */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class JsonBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  @Param(Array("5k", "100k"))
  var size: String = ""
  var text: String = ""
  var parse: String => Either[Any, Json] = null

  @Setup def setup(): Unit = {
    val record = """{"name":"Ann","age":41,"tags":["a","b"],"born":"1985-03-09","email":null,"score":12.5,"ok":true}"""
    val n = if (size == "5k") 50 else 1000
    text = "[" + List.fill(n)(record).mkString(",") + "]"
    val fx: Runner = if (impl == "machine") Machine else okay.effects.free.Free
    val streams = new Streams[fx.type]()(fx)
    val js = new Jsons[fx.type, streams.type](streams)
    parse = s => fx.run(js.parser(s))
  }

  @Benchmark def effects: Boolean = parse(text).isRight
  @Benchmark def descent: Boolean = Descent.parse(text).isRight
}
