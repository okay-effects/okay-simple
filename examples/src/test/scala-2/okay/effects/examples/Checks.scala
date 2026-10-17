package okay.effects.examples

import okay.effects.Runner

/** the Scala 2 examples' values on the implementation `fx` */
abstract class Checks(val fx: Runner) extends SharedChecks(fx) {
  private val basics = new Basics2[Runner]()(fx)

  test("the basics, Scala 2's way: rows by names, an effect's operations with their answers") {
    assertEquals(basics.greeted, (1, (List("hello, Ann"), 0)))
    assertEquals(basics.counted, (102, (2, 2)))
    assertEquals(basics.firstEven(List(1, 3, 4, 6)), Some(40))
  }
}
