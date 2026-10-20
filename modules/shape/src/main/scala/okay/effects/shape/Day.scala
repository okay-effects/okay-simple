package okay.effects.shape

/** a calendar date, ISO `yyyy-mm-dd` on the wire — our own, so that Scala.js and Native need no java.time */
final case class Day(year: Int, month: Int, day: Int) extends Ordered[Day] {
  override def toString: String = f"$year%04d-$month%02d-$day%02d"
  def compare(that: Day): Int = {
    val y = year.compare(that.year); if (y != 0) y else { val m = month.compare(that.month); if (m != 0) m else day.compare(that.day) }
  }
}
object Day {
  private val Iso = """(\d{4})-(\d{2})-(\d{2})""".r
  def isLeap(y: Int): Boolean = (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
  def daysIn(y: Int, m: Int): Int = m match {
    case 2 => if (isLeap(y)) 29 else 28
    case 4 | 6 | 9 | 11 => 30
    case _ => 31
  }
  def valid(y: Int, m: Int, d: Int): Boolean = m >= 1 && m <= 12 && d >= 1 && d <= daysIn(y, m)
  def of(y: Int, m: Int, d: Int): Either[String, Day] =
    if (valid(y, m, d)) Right(Day(y, m, d)) else Left(s"not a date: $y-$m-$d")
  def parse(s: String): Either[String, Day] = s match {
    case Iso(y, m, d) => of(y.toInt, m.toInt, d.toInt)
    case _ => Left(s"a date is yyyy-mm-dd, not '$s'")
  }
}
