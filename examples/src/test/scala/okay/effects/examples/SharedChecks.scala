package okay.effects.examples

import okay.effects.Runner

/** the shared examples' values on the implementation `fx`, both compilers */
abstract class SharedChecks(fx: Runner) extends munit.FunSuite {
  private implicit val implementation: Runner = fx

  test("code written once for both compilers") {
    assertEquals(new Shared().served(List("Ann", "Bob")), (List("1: Ann", "2: Bob"), ()))
  }
}
