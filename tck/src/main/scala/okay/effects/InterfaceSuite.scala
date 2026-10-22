package okay.effects

/** the interface's suite, compiled by both compilers, on each implementation */
abstract class InterfaceSuite(fx: okay.effects.Runner) extends munit.FunSuite {
  val p = new Programs(fx)
  val g = new Numbering(fx)
  val d = new ShiftReset(fx)
  val at = new AnswerTypes(fx)
  val more = new MoreStd(fx)
  val u = new Utilities(fx)
  val ic = new Interception(fx)
  val tc = new TailCalls(fx)

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
  test("the cut — once, a cut back to a mark, a failure after a cut not coming back, a long chain") {
    assertEquals(more.onceAll, List(1))
    assertEquals(more.cutInner, List(11, 12))
    assertEquals(more.failAfterCut, None)
    assertEquals(more.failNoCut, Some(2))
    assertEquals(more.markCut, List(11, 12))
    assertEquals(more.chainCut(1000000), Some(1000000))
  }
  test("lift and lower: another monad's programs inside, the program given back as one of it") {
    assertEquals(more.lowered(Some(2)), Some(3))
    assertEquals(more.lowered(None), None)
  }
  test("recover: what was raised to a program in its place") {
    assertEquals(more.recovered, 4)
    assertEquals(more.unraised, 7)
  }
  test("local: the environment changed inside, the same outside; in a row of more") {
    assertEquals(u.locally, (10, 20, 10))
    assertEquals(u.localInRow, (List("e=42"), 42))
  }
  test("listen and censor: what was told inside, told outside too") {
    assertEquals(u.listened, (List("a", "b", "c"), (List("b", "c"), 3)))
    assertEquals(u.censored, (List("a", "B"), 1))
  }
  test("gets, getAndSet, guard") {
    assertEquals(u.stateWords, (7, (50, 5)))
    assertEquals(u.guarded, List(3, 6, 9))
  }
  test("abort: the rest dropped, the state outside kept") {
    assertEquals(u.aborted, (1, None))
    assertEquals(u.notAborted, Some(3))
  }
  test("fresh numbers, and in each branch the supply of its time") {
    assertEquals(u.freshOnes, List(100L, 101L, 102L))
    assertEquals(u.freshEachBranch, List((1, 1L), (2, 1L)))
    assertEquals(u.freshThroughBranches, List((1, 1L), (2, 2L)))
  }
  test("random from a seed: the same seed, the same numbers") {
    assertEquals(u.randoms(42), u.randoms(42))
    assert(u.randoms(42) != u.randoms(43))
    assert(u.randoms(42).forall(n => n >= 0 && n < 100))
    assert(u.doubleInRange)
  }
  test("a store by key") {
    assertEquals(u.stored, (Map("b" -> 2), (None, Some(2))))
  }
  test("intercept: the operations observed, passed on, the effect working as it did") {
    assertEquals(ic.observed, (List("put 1", "get", "put 11", "get"), (11, 11)))
  }
  test("intercept: two interceptors, the inner first") {
    assertEquals(ic.nested._1.take(4), List("inner", "outer", "inner", "outer"))
    assertEquals(ic.nested._2, (11, 11))
  }
  test("intercept: an operation changed, refused, answered here") {
    assertEquals(ic.doubled, 42)
    assertEquals(ic.evensOnly, List(2, 4, 6))
    assertEquals(ic.answeredHere, (5, 100))
  }
  test("intercept in constant stack: 100 000 operations") {
    assertEquals(ic.many(100000), (100000, 200000))
  }
  test("mutual tail recursion, a million calls, in constant stack; with effects; a loop") {
    assertEquals(tc.isEven(1000000), true)
    assertEquals(tc.isOdd(1000001), true)
    assertEquals(tc.counted(1000000), (1000000, true))
    assertEquals(tc.summed(100000), (100001, 5000050000L))
  }
  test("constant stack: 100 000 operations under three handlers, 100 000 binds") {
    assertEquals(p.counted(100000), (List(), (100000, ())))
    assertEquals(p.binds(100000), 100000)
  }
}
