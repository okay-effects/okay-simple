package okay.api.compat

import okay.simple.machine.Simple

/** Scala 2.13's effect, handler and program, on the Scala 2.13 build of the machine */
class Scala2Suite extends munit.FunSuite {
  test("Scala 2.13: an effect, its handler and a program of its own") {
    assertEquals(new Counting(Simple).greeted(List("Ann", "Bob")), (List("hello, Ann", "hello, Bob"), (2, 2)))
  }
}
