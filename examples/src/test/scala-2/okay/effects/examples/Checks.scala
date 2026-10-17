package okay.effects.examples

import okay.effects.Effects

/** the Scala 2 examples' values on the implementation `fx` */
abstract class Checks(val fx: Effects) extends SharedChecks(fx) {
  private val basics = new Basics2[Effects]()(fx)

  test("the basics, Scala 2's way: rows by names, an effect's operations with their answers") {
    assertEquals(basics.greeted, (1, (List("hello, Ann"), 0)))
    assertEquals(basics.counted, (102, (2, 2)))
    assertEquals(basics.firstEven(List(1, 3, 4, 6)), Some(40))
  }
}
