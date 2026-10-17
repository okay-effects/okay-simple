package okay.effects.examples

import okay.effects.Effects

/** the shared examples' values on the implementation `fx`, both compilers */
abstract class SharedChecks(fx: Effects) extends munit.FunSuite {
  test("code written once for both compilers") {
    assertEquals(new Shared(fx).served(List("Ann", "Bob")), (List("1: Ann", "2: Bob"), ()))
  }
}
