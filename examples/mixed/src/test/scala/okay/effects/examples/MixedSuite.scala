package okay.effects.examples

import okay.effects.Effects
import okay.effects.machine.Machine
import okay.effects.free.Free

/** the mixed application's values, on the machine and on the `Free` monad (their Scala 2.13 builds) */
abstract class MixedChecks(fx: Effects) extends munit.FunSuite:
  test("Scala 2.13's effect, handler and program beside Scala 3's, one row, one run"):
    val m = Mixed(using fx)
    assertEquals(m.both, (4, (2, 2)))
    assertEquals(m.served, (List("1: Ann"), ()))

class MixedOnMachine extends MixedChecks(Machine)
class MixedOnFree extends MixedChecks(Free)
