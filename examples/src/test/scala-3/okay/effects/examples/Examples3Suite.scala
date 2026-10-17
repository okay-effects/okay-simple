package okay.effects.examples

import okay.effects.simple.Machine
import okay.effects.simple.free.Free

/** the examples' values, Scala 3: a change that breaks an example breaks this */
class Examples3Suite extends munit.FunSuite:
  test("the basics: a row of three, an effect of one's own, a general handler"):
    assertEquals(Basics3.greeted, (1, (List("hello, Ann"), 0)))
    assertEquals(Basics3.counted, (102, (2, 2)))
    assertEquals(Basics3.twoFlips, List((true, true), (true, false), (false, true), (false, false)))

  test("control: generators, a search, errors recovered, shift and reset, the answer type changed"):
    assertEquals(Control3.collected, (List(3, 2, 1), ()))
    assertEquals(Control3.firstTwo, (List(5, 4), None))
    assertEquals(Control3.summed, (10, ()))
    assertEquals(Control3.pythagorean(20), Some((3, 4, 5)))
    assertEquals(Control3.halves, (5, 8))
    assertEquals(Control3.twice, 12)
    assertEquals(Control3.greeting, "hello, Ann")

  test("one program, any implementation"):
    assertEquals(Implementations3.onMachine, (4, "Ann #1"))
    assertEquals(Implementations3.onFree, Implementations3.onMachine)
    assertEquals(Implementations3.onFreeAsItIs, 40)

  test("code written once for both compilers, on each implementation"):
    assertEquals(Shared(Machine).served(List("Ann", "Bob")), (List("1: Ann", "2: Bob"), ()))
    assertEquals(Shared(Free).served(List("Ann", "Bob")), (List("1: Ann", "2: Bob"), ()))
