package okay.effects.json

/** A JSON VALUE: a datum any face and any transport speaks. An `Obj` is ORDERED: the order a wire writes, a form shows */
sealed trait Json
object Json {
  case object Null extends Json
  final case class Bool(b: Boolean) extends Json
  final case class Num(n: BigDecimal) extends Json
  final case class Str(s: String) extends Json
  final case class Arr(items: Vector[Json]) extends Json
  final case class Obj(fields: Vector[(String, Json)]) extends Json {
    def get(name: String): Option[Json] = fields.collectFirst { case (n, v) if n == name => v }
    def has(name: String): Boolean = fields.exists(_._1 == name)
    def without(name: String): Obj = Obj(fields.filterNot(_._1 == name))
    def +:(field: (String, Json)): Obj = Obj(field +: fields)
  }
  object Obj {
    val empty: Obj = Obj(Vector.empty)
    def of(fields: (String, Json)*): Obj = Obj(fields.toVector)
  }

  def num(i: Int): Json = Num(BigDecimal(i))
  def num(l: Long): Json = Num(BigDecimal(l))
  def num(n: BigInt): Json = Num(BigDecimal(n))
  def str(s: String): Json = Str(s)
  def bool(b: Boolean): Json = Bool(b)
  def arr(items: Json*): Json = Arr(items.toVector)
  def obj(fields: (String, Json)*): Obj = Obj(fields.toVector)

  /** a word for what a value is, for a message */
  def kind(v: Json): String = v match {
    case Null => "null"
    case Bool(_) => "a boolean"
    case Num(_) => "a number"
    case Str(_) => "text"
    case Arr(_) => "a list"
    case Obj(_) => "a record"
  }

  // ---- text OF a value; the text TO a value is the parser's, `Jsons`
  def print(v: Json): String = { val b = new StringBuilder; write(v, b, -1, 0); b.toString }
  def pretty(v: Json, indent: Int = 2): String = { val b = new StringBuilder; write(v, b, indent, 0); b.toString }

  private def write(v: Json, b: StringBuilder, indent: Int, depth: Int): Unit = v match {
    case Null => b.append("null")
    case Bool(x) => b.append(if (x) "true" else "false")
    case Num(n) => b.append(n.bigDecimal.toPlainString)
    case Str(s) => quote(s, b)
    case Arr(items) =>
      if (items.isEmpty) b.append("[]")
      else {
        b.append('['); var first = true
        items.foreach { x => if (!first) b.append(','); first = false; newline(b, indent, depth + 1); write(x, b, indent, depth + 1) }
        newline(b, indent, depth); b.append(']')
      }
    case Obj(fields) =>
      if (fields.isEmpty) b.append("{}")
      else {
        b.append('{'); var first = true
        fields.foreach { case (k, x) =>
          if (!first) b.append(','); first = false; newline(b, indent, depth + 1)
          quote(k, b); b.append(':'); if (indent >= 0) b.append(' '); write(x, b, indent, depth + 1)
        }
        newline(b, indent, depth); b.append('}')
      }
  }
  private def newline(b: StringBuilder, indent: Int, depth: Int): Unit =
    if (indent >= 0) { b.append('\n'); var i = 0; while (i < indent * depth) { b.append(' '); i += 1 } }
  private def quote(s: String, b: StringBuilder): Unit = {
    b.append('"')
    s.foreach {
      case '"' => b.append("\\\"")
      case '\\' => b.append("\\\\")
      case '\n' => b.append("\\n")
      case '\r' => b.append("\\r")
      case '\t' => b.append("\\t")
      case '\b' => b.append("\\b")
      case '\f' => b.append("\\f")
      case c if c < 0x20 => b.append(f"\\u${c.toInt}%04x")
      case c => b.append(c)
    }
    b.append('"')
  }
}
