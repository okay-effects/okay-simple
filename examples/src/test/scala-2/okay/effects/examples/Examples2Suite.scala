package okay.effects.examples

import okay.effects.simple.Machine

/** the examples' values, Scala 2 */
class Examples2Suite extends munit.FunSuite {
  test("the basics, Scala 2's way: rows by names, an effect's operations with their answers") {
    assertEquals(Basics2.greeted, (1, (List("hello, Ann"), 0)))
    assertEquals(Basics2.counted, (102, (2, 2)))
    assertEquals(Basics2.firstEven(List(1, 3, 4, 6)), Some(40))
  }
  test("code written once for both compilers") {
    assertEquals(new Shared(Machine).served(List("Ann", "Bob")), (List("1: Ann", "2: Bob"), ()))
  }
}
