package okay.effects.time

/** dates and moments with no java.time: printed, read, the calendar's corners */
class TimeSuite extends munit.FunSuite {
  test("a moment printed and read, offsets and fractions") {
    val t = Instant.parse("2026-11-04T14:01:05Z").toOption.get
    assertEquals(t.seconds, 1793800865L)
    assertEquals(t.toString, "2026-11-04T14:01:05Z")
    assertEquals(Instant.parse("2026-11-04T16:01:05+02:00"), Right(t))
    assertEquals(Instant.parse("2026-11-04 09:01:05-0500"), Right(t))
    assertEquals(Instant.parse("2026-11-04T14:01:05.120Z").map(_.toString), Right("2026-11-04T14:01:05.120Z"))
    assertEquals(Instant.parse("2026-11-04T14:01"), Right(t.copy(seconds = t.seconds - 5)))
    assertEquals(Instant.parse("1969-12-31T23:59:59.5Z").map(i => (i.seconds, i.nanos, i.millis)),
      Right((-1L, 500000000, -500L)))
    assert(Instant.parse("2026-02-30T00:00:00Z").isLeft)
    assert(Instant.parse("2026-11-04T24:00:00Z").isLeft)
  }
  test("found in a log line, whatever its format") {
    val t = Instant.parse("2026-11-04T14:01:05Z").toOption
    assertEquals(Instant.find("""{"ts": "2026-11-04T14:01:05.120Z", "msg": "x"}""").map(_.seconds), t.map(_.seconds))
    assertEquals(Instant.find("ts=2026-11-04T14:01:05Z level=error"), t)
    assertEquals(Instant.find("10.0.0.1 - - [2026-11-04T16:01:05+02:00] \"GET /\""), t)
    assertEquals(Instant.find("no time here"), None)
  }
  test("the calendar: leap years, days of the week, epoch days both ways") {
    assertEquals(Date(2024, 2, 29).plusDays(1), Date(2024, 3, 1))
    assertEquals(Date(1900, 2, 28).plusDays(1), Date(1900, 3, 1))
    assertEquals(Date(2000, 2, 28).plusDays(1), Date(2000, 2, 29))
    assertEquals(Date(1970, 1, 1).epochDay, 0L)
    assertEquals(Date(2026, 11, 4).dayOfWeek, 3)
    assertEquals(Date.parse("2026-11-04"), Right(Date(2026, 11, 4)))
    assert(Date.parse("2026-13-01").isLeft)
    (-800000L to 800000L by 997L).foreach(d => assertEquals(Date.ofEpochDay(d).epochDay, d))
  }
}
