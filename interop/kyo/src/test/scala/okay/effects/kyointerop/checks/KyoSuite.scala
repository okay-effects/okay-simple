package okay.effects.kyointerop.checks

import kyo.{<, Abort, Var, Kyo}
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free
import okay.effects.kyointerop.KyoEffects

abstract class KyoChecks(val fx0: Runner) extends munit.FunSuite:
  given runner: Runner = fx0
  val k: KyoEffects[runner.type, okay.effects.stream.Streams[runner.type]] = KyoEffects()
  import k.*, k.fx.*, k.streams.*

  type V = Kyos[Var[Int]]#L
  test("kyo's Var inside a program; the program lowered to kyo, kyo's handler runs it") {
    val p: Int ! V = fromKyo[Int, Var[Int], V](Var.update[Int](_ + 1)).flatMap(a =>
      fromKyo[Int, Var[Int], V](Var.update[Int](_ * 10)).map(_ + a))
    assertEquals(Var.run(1)(toKyo(p)).eval, 22)
  }
  type E = Kyos[Abort[String]]#L
  test("kyo's Abort: the program's failure kyo's") {
    val p: Int ! E = fromKyo[Int, Abort[String], E](Abort.fail("boom")).map(_ + 1)
    assertEquals(Abort.run[String](toKyo(p)).eval.toString, "Failure(boom)")
  }
  test("10 000 kyo computations in turn, in constant stack") {
    def loop(n: Int, acc: Int): Int ! V =
      if n == 0 then pure(acc) else fromKyo[Int, Var[Int], V](Var.update[Int](_ + 1)).flatMap(_ => loop(n - 1, acc + 1))
    assertEquals(Var.run(0)(toKyo(loop(10000, 0))).eval, 10000)
  }
  test("a stream of kyo's computations a kyo Stream, and back") {
    val s: Stream[Int, V] = range[V](1, 4).evalMap(n => fromKyo[Int, Var[Int], V](Var.update[Int](_ + n)))
    assertEquals(Var.run(0)(toKyoStream(s).run).eval.toList, List(1, 3, 6))
    val back = fromKyoStream[Int, Any](kyo.Stream.init(Seq(5, 6, 7))).map(_ * 2).toList
    assertEquals(toKyo[List[Int], Any](back).eval, List(10, 12, 14))
  }

class KyoOnMachine extends KyoChecks(Machine)
class KyoOnFree extends KyoChecks(Free)
class KyoOnTagless extends KyoChecks(okay.effects.tagless.Tagless.on(Machine))
