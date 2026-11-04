package okay.effects.time

/** THE LAW, where java.time is: the same text and the same seconds as `java.time.Instant`, over random moments */
class AgainstJavaTimeSuite extends munit.FunSuite {
  test("printed and read as java.time does") {
    val rnd = new scala.util.Random(11)
    (1 to 20000).foreach { _ =>
      val s = rnd.nextLong() % 253402300799L // within years 0000…9999
      val n = if (rnd.nextBoolean()) 0 else rnd.nextInt(1000000000)
      val j = java.time.Instant.ofEpochSecond(s, n.toLong)
      val ours = Instant(s, n)
      if (s >= -62167219200L) assertEquals(ours.toString, j.toString)
      assertEquals(Instant.parse(j.toString), Right(ours), j.toString)
      val d = j.atOffset(java.time.ZoneOffset.UTC).toLocalDate
      assertEquals(ours.date, Date(d.getYear, d.getMonthValue, d.getDayOfMonth))
      assertEquals(ours.date.dayOfWeek, d.getDayOfWeek.getValue)
    }
  }
}
