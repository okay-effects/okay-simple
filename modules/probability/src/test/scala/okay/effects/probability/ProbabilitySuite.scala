package okay.effects.probability

import okay.effects.{Runner, Sum}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ProbabilityChecks(val fx0: Runner) extends munit.FunSuite {
  val pr = Probability(fx0)
  import pr._
  import pr.fx._
  import pr.fx.std._

  def dice: Int ! Dist = uniform[Int, Dist]((1 to 6).toList).flatMap(a => uniform[Int, Dist]((1 to 6).toList).map(b => a + b))

  test("exact: two dice, each sum's probability") {
    val d = fx0.run(exact(dice)).toMap
    assertEqualsDouble(d(7), 6.0 / 36, 1e-12)
    assertEqualsDouble(d(2), 1.0 / 36, 1e-12)
    assertEqualsDouble(d.values.sum, 1.0, 1e-12)
  }
  test("exact: a biased coin, twice, at least one head") {
    val d = fx0.run(exact(flip[Dist](0.3).flatMap(a => flip[Dist](0.3).map(b => a || b)))).toMap
    assertEqualsDouble(d(true), 1 - 0.7 * 0.7, 1e-12)
  }
  type DR[X] = Sum[Random, Dist, X]
  def diceIn: Int ! DR = uniform[Int, DR]((1 to 6).toList).flatMap(a => uniform[Int, DR]((1 to 6).toList).map(b => a + b))
  test("sampled: from a seed, the same; in range; the mean near 7") {
    def sample(seed: Long, n: Int): List[Int] =
      fx0.run(random(seed)((1 to n).toList.foldLeft(pure[List[Int], Random](Nil))((p, _) => p.flatMap(l => sampled(diceIn).map(_ :: l)))))
    assertEquals(sample(1, 20), sample(1, 20))
    val xs = sample(7, 4000)
    assert(xs.forall(x => x >= 2 && x <= 12))
    assertEqualsDouble(xs.sum.toDouble / xs.size, 7.0, 0.2)
  }
}

class ProbabilityOnMachine extends ProbabilityChecks(Machine)
class ProbabilityOnFree extends ProbabilityChecks(Free)
