package okay.api

/** the interface's suite, compiled by both compilers, on each implementation */
abstract class InterfaceSuite(fx: Std) extends munit.FunSuite {
  val p = new Programs(fx)

  test("a program's effects handled in the row's order") {
    assertEquals(p.handledInOrder, (13, (List("hello, Ann"), 10)))
  }
  test("handled out of order: split takes an effect from any place") {
    assertEquals(p.handledOutOfOrder, (13, (List("hello, Ann"), 10)))
  }
  test("the same program in a row of another order") {
    assertEquals(p.anotherRow, (List("hello, Ann"), (13, 10)))
  }
  test("a program of a smaller row embedded in a bigger one") {
    assertEquals(p.grown, (List(), (5, 5)))
  }
  test("multi-shot k: one state through every branch") {
    assertEquals(p.stateOutside, (6, List(10, 20, 30)))
  }
  test("multi-shot k: each branch its own state") {
    assertEquals(p.stateInside, List((1, 10), (2, 20), (3, 30)))
  }
  test("an error: the value, or what was raised") {
    assertEquals(p.divided(2), (1, Right(5)))
    assertEquals(p.divided(0), (0, Left("div by zero")))
    assertEquals(p.stateKeptOnRaise, Left("div by zero"))
  }
  test("a split on its own, then a general handler") {
    assertEquals(p.splitThenHandle, (1, 40))
  }
  test("constant stack: 100 000 operations under three handlers, 100 000 binds") {
    assertEquals(p.counted(100000), (List(), (100000, ())))
    assertEquals(p.binds(100000), 100000)
  }
}
