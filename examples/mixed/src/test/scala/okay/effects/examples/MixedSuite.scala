package okay.effects.examples

/** the mixed application's values */
class MixedSuite extends munit.FunSuite:
  test("Scala 2.13's effect, handler and program beside Scala 3's, one row, one run"):
    assertEquals(Mixed.both, (4, (2, 2)))
    assertEquals(Mixed.served, (List("1: Ann"), ()))
