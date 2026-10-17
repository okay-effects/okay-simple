package okay.effects

/** the interface's suite, compiled by both compilers, on each implementation */
abstract class InterfaceSuite(fx: okay.effects.Runner) extends munit.FunSuite {
  val p = new Programs(fx)
  val g = new Numbering(fx)
  val d = new ShiftReset(fx)
  val at = new AnswerTypes(fx)
  val more = new MoreStd(fx)

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
  test("a stateful general handler: k resumed with the next state, each branch its own") {
    assertEquals(g.numbered, List((2, 1), (12, 11)))
    assertEquals(g.numberedInState, (7, List((2, (false, 5, 1)), (12, (true, 6, 11)))))
    assertEquals(g.stopped, List((1, 0)))
  }
  test("shift and reset: k resumed twice, dropped, each resumption its own") {
    assertEquals(d.twice, 12)
    assertEquals(d.dropped, 42)
    assertEquals(d.both, List(10, 20))
  }
  test("delimited control changing answer types") {
    assertEquals(at.greeting, "hello, Ann")
    assertEquals(at.twice, 12)
    assertEquals(at.lifted, 3)
    assertEquals(at.binds(100000), 100000)
  }
  test("a generator: collected, taken, each value to an effectful consumer") {
    assertEquals(more.collected, (List(3, 2, 1), ()))
    assertEquals(more.taken, (List(5, 4), None))
    assertEquals(more.all3, (List(3, 2, 1), Some(())))
    assertEquals(more.summed, (6, ()))
  }
  test("the first way with no fail; all the ways, linear") {
    assertEquals(more.evens(List(1, 2, 3, 4)), Some(20))
    assertEquals(more.evens(List(1, 3)), None)
    assertEquals(more.many(3000), 3000)
  }
  test("lift and lower: another monad's programs inside, the program given back as one of it") {
    assertEquals(more.lowered(Some(2)), Some(3))
    assertEquals(more.lowered(None), None)
  }
  test("recover: what was raised to a program in its place") {
    assertEquals(more.recovered, 4)
    assertEquals(more.unraised, 7)
  }
  test("constant stack: 100 000 operations under three handlers, 100 000 binds") {
    assertEquals(p.counted(100000), (List(), (100000, ())))
    assertEquals(p.binds(100000), 100000)
  }
}
