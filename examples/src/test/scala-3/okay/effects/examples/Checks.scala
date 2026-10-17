package okay.effects.examples

import okay.effects.Effects

/** the Scala 3 examples' values on the implementation `fx`: a change that breaks an example breaks this */
abstract class Checks(fx: Effects) extends SharedChecks(fx):
  val basics = Basics3(fx)
  val control = Control3(fx)

  test("the basics: a row of three, an effect of one's own, a general handler"):
    assertEquals(basics.greeted, (1, (List("hello, Ann"), 0)))
    assertEquals(basics.counted, (102, (2, 2)))
    assertEquals(basics.twoFlips, List((true, true), (true, false), (false, true), (false, false)))

  test("control: generators, a search, errors recovered, shift and reset, the answer type changed"):
    assertEquals(control.collected, (List(3, 2, 1), ()))
    assertEquals(control.firstTwo, (List(5, 4), None))
    assertEquals(control.summed, (10, ()))
    assertEquals(control.pythagorean(20), Some((3, 4, 5)))
    assertEquals(control.halves, (5, 8))
    assertEquals(control.twice, 12)
    assertEquals(control.greeting, "hello, Ann")
