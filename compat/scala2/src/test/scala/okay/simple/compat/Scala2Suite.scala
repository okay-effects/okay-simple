package okay.simple.compat

import okay.simple.free.FreeEffects

/** Scala 2.13's program run on the implementation Scala 3 built */
class Scala2Suite extends munit.FunSuite {
  test("Scala 2.13: an effect, its handler and a program of its own, on Scala 3's build") {
    assertEquals(new Counting(FreeEffects.effects).greeted(List("Ann", "Bob")), (List("hello, Ann", "hello, Bob"), (2, 2)))
  }
}
