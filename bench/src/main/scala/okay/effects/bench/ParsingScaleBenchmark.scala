package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{
  Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState
}
import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.json.{Descent, Json, Jsons}
import okay.effects.parsing.{Expect, Input, Parsing}
import okay.effects.stream.Streams

/** HOW PARSING SCALES: the same work at sizes doubling, so a time that more than doubles with the size shows at once —
 * linear is a flat time per item across `n`. `sbt "bench/Jmh/run -f 1 -wi 2 -i 3 ParsingScaleBenchmark"`.
 *  - `descent`, `grammar`: a JSON array of `n` records, by the hand parser and by the grammar on effects;
 *  - `stream`: the same text in 4 KB chunks, its values as they come (`Jsons.values`), the stream input remembered;
 *  - `deep`: `n` nested arrays, by the grammar (no stack: the heap's);
 *  - `termsOrElse`, `termsCommitted`: `n` terms `+1`, by `many` (backtracking) and `manyCommitted` (cut as taken).
 * The history is in bench.d/ */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ParsingScaleBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  @Param(Array("1000", "2000", "4000", "8000"))
  var n: Int = 0

  var text: String = ""
  var deepText: String = ""
  var terms: String = ""
  var grammar: String => Either[Any, Json] = null
  var deepGrammar: String => Either[Any, Json] = null
  var stream: String => Int = null
  var runOrElse: String => Either[Any, Int] = null
  var runCommitted: String => Either[Any, Int] = null

  @Setup def setup(): Unit = {
    val record = """{"name":"Ann","age":41,"tags":["a","b"],"born":"1985-03-09","email":null,"score":12.5,"ok":true}"""
    text = "[" + List.fill(n)(record).mkString(",") + "]"
    deepText = "[" * n + "1" + "]" * n
    terms = "1" + "+1" * n
    val fx: Runner = if (impl == "machine") okay.effects.machine.Machine else okay.effects.free.Free
    val streams = new Streams[fx.type]()(fx)
    val js = new Jsons[fx.type, streams.type](streams)
    grammar = s => fx.run(js.parser(s))
    deepGrammar = s => fx.run(js.parser(s, maxDepth = Int.MaxValue))
    stream = s => fx.run(js.values[Void](streams.fromIterator[String, Void](s.grouped(4096))).fold(0)((k, _) => k + 1))
    val ps = new Parsing[fx.type]()(fx)
    import ps._
    import ps.fx._
    type P[X] = Sum[({ type L[x] = Sum[({ type M[y] = Sum[Void, Std.Choose, y] })#M, Expect, x] })#L, Input, X]
    def one: Int ! P = char[P]('1').as(1)
    def plusOne: Int ! P = char[P]('+').flatMap(_ => one)
    val byOrElse: Int ! P = one.flatMap(a => many[Int, P](plusOne).map(as => a + as.sum))
    val byCommitted: Int ! P = one.flatMap(a => manyCommitted[Int, P](plusOne).map(as => a + as.sum))
    runOrElse = s => fx.run(parse(s)(byOrElse))
    runCommitted = s => fx.run(parse(s)(byCommitted))
  }

  @Benchmark def descent: Boolean = Descent.parse(text).isRight
  @Benchmark def grammarJson: Boolean = grammar(text).isRight
  @Benchmark def streamJson: Int = stream(text)
  @Benchmark def deep: Boolean = deepGrammar(deepText).isRight
  @Benchmark def termsOrElse: Boolean = runOrElse(terms).isRight
  @Benchmark def termsCommitted: Boolean = runCommitted(terms).isRight
}
