package okay.effects.time

/** A CALENDAR DATE, proleptic Gregorian, as ISO-8601 writes it (`2026-11-04`) — our own, so that Scala.js and Native
 * need no java.time. Days from the epoch by Howard Hinnant's algorithms (`days_from_civil`, `civil_from_days`) */
final case class Date(year: Int, month: Int, day: Int) extends Ordered[Date] {
  override def toString: String =
    if (year >= 0 && year <= 9999) f"$year%04d-$month%02d-$day%02d"
    else f"${if (year < 0) "-" else "+"}${math.abs(year)}%04d-$month%02d-$day%02d"
  def compare(that: Date): Int = epochDay.compare(that.epochDay)
  /** days since 1970-01-01 */
  def epochDay: Long = Date.daysFromCivil(year.toLong, month, day)
  def plusDays(n: Long): Date = Date.ofEpochDay(epochDay + n)
  /** 1 Monday … 7 Sunday */
  def dayOfWeek: Int = (Math.floorMod(epochDay + 3, 7L) + 1).toInt
}

object Date {
  def isLeap(y: Int): Boolean = (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
  def daysIn(y: Int, m: Int): Int = m match {
    case 2 => if (isLeap(y)) 29 else 28
    case 4 | 6 | 9 | 11 => 30
    case _ => 31
  }
  def valid(y: Int, m: Int, d: Int): Boolean = m >= 1 && m <= 12 && d >= 1 && d <= daysIn(y, m)
  def of(y: Int, m: Int, d: Int): Either[String, Date] =
    if (valid(y, m, d)) Right(Date(y, m, d)) else Left(s"not a date: $y-$m-$d")

  private[time] def daysFromCivil(y0: Long, m: Int, d: Int): Long = {
    val y = if (m <= 2) y0 - 1 else y0
    val era = (if (y >= 0) y else y - 399) / 400
    val yoe = y - era * 400
    val doy = (153 * (if (m > 2) m - 3 else m + 9) + 2) / 5 + d - 1
    val doe = yoe * 365 + yoe / 4 - yoe / 100 + doy
    era * 146097 + doe - 719468
  }

  def ofEpochDay(days: Long): Date = {
    val z = days + 719468
    val era = (if (z >= 0) z else z - 146096) / 146097
    val doe = z - era * 146097
    val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
    val mp = (5 * doy + 2) / 153
    val d = (doy - (153 * mp + 2) / 5 + 1).toInt
    val m = (if (mp < 10) mp + 3 else mp - 9).toInt
    Date((yoe + era * 400 + (if (m <= 2) 1 else 0)).toInt, m, d)
  }

  private val Text = """([+-]?\d{4,})-(\d\d)-(\d\d)""".r
  def parse(s: String): Either[String, Date] = s.trim match {
    case Text(y, m, d) => of(y.toInt, m.toInt, d.toInt)
    case _ => Left(s"a date is yyyy-mm-dd, not '$s'")
  }
}

/**
 * A MOMENT, as seconds and nanoseconds since 1970-01-01T00:00:00Z — java.time's `Instant`, what every platform has:
 * printed as ISO-8601 in UTC (`2026-11-04T14:01:05Z`, the fraction when there is one), read from ISO-8601 with any
 * zone offset (`Z`, `+02:00`, `+0200`, `-05`; none is UTC), a space allowed for the `T`, a fraction of any length.
 */
final case class Instant(seconds: Long, nanos: Int = 0) extends Ordered[Instant] {
  require(nanos >= 0 && nanos < 1000000000, s"nanoseconds 0 to 999999999, not $nanos")
  def compare(that: Instant): Int = {
    val s = seconds.compare(that.seconds); if (s != 0) s else nanos.compare(that.nanos)
  }
  def millis: Long = seconds * 1000 + nanos / 1000000
  def plusSeconds(n: Long): Instant = copy(seconds = seconds + n)
  def date: Date = Date.ofEpochDay(Math.floorDiv(seconds, 86400L))
  /** the time of day in UTC: hour, minute, second */
  def clock: (Int, Int, Int) = {
    val s = Math.floorMod(seconds, 86400L)
    ((s / 3600).toInt, (s % 3600 / 60).toInt, (s % 60).toInt)
  }
  override def toString: String = {
    val (h, m, s) = clock
    val fraction =
      if (nanos == 0) ""
      else if (nanos % 1000000 == 0) f".${nanos / 1000000}%03d"
      else if (nanos % 1000 == 0) f".${nanos / 1000}%06d"
      else f".$nanos%09d"
    f"$date%sT$h%02d:$m%02d:$s%02d$fraction%sZ"
  }
}

object Instant {
  val epoch: Instant = Instant(0)
  def ofSeconds(s: Long): Instant = Instant(s)
  def ofMillis(ms: Long): Instant = Instant(Math.floorDiv(ms, 1000L), (Math.floorMod(ms, 1000L) * 1000000).toInt)
  def of(date: Date, hour: Int = 0, minute: Int = 0, second: Int = 0, nanos: Int = 0): Instant =
    Instant(date.epochDay * 86400 + hour * 3600L + minute * 60L + second, nanos)

  private val Pattern =
    """([+-]?\d{4,})-(\d\d)-(\d\d)[Tt ](\d\d):(\d\d)(?::(\d\d)(?:[.,](\d{1,9})\d*)?)?\s*(Z|z|[+-]\d\d(?::?\d\d)?)?""".r

  private def read(m: scala.util.matching.Regex.Match): Either[String, Instant] = {
    val y = m.group(1).toInt; val mo = m.group(2).toInt; val d = m.group(3).toInt
    val h = m.group(4).toInt; val mi = m.group(5).toInt; val s = Option(m.group(6)).fold(0)(_.toInt)
    val nanos = Option(m.group(7)).fold(0)(f => (f + "000000000").take(9).toInt)
    if (!Date.valid(y, mo, d)) Left(s"not a date: ${m.group(1)}-${m.group(2)}-${m.group(3)}")
    else if (h > 23 || mi > 59 || s > 59) Left(s"not a time of day: ${m.group(4)}:${m.group(5)}")
    else {
      val offset = Option(m.group(8)).filterNot(z => z == "Z" || z == "z").fold(0) { z =>
        val sign = if (z.startsWith("-")) -1 else 1
        val digits = z.drop(1).replace(":", "")
        sign * (digits.take(2).toInt * 3600 + (if (digits.length > 2) digits.drop(2).toInt * 60 else 0))
      }
      Right(Instant(Date(y, mo, d).epochDay * 86400 + h * 3600L + mi * 60L + s - offset, nanos))
    }
  }

  /** a text that is an ISO-8601 moment */
  def parse(text: String): Either[String, Instant] =
    Pattern.unapplySeq(text.trim) match {
      case Some(_) => read(Pattern.findFirstMatchIn(text.trim).get)
      case None => Left(s"an ISO-8601 time is yyyy-mm-ddThh:mm[:ss[.fff]][Z|±hh:mm], not '$text'")
    }

  /** the first ISO-8601 moment in a text — a log line's, wherever it stands */
  def find(text: String): Option[Instant] =
    Pattern.findAllMatchIn(text).map(read).collectFirst { case Right(i) => i }
}
