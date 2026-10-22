package okay.effects.json

/** A JSON VALUE: a datum any face and any transport speaks. An `Obj` is ORDERED: the order a wire writes, a form shows
 */
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

  // ---- text OF a value; the text TO a value is the parser's, `Jsons`. ITERATIVE, an explicit stack of what is still
  // to write, so a value nested a hundred thousand deep (the grammar parses one) prints, no stack overflow
  def print(v: Json, ascii: Boolean = false): String = {
    val b = new java.lang.StringBuilder; write(v, b, -1, ascii); b.toString
  }
  def pretty(v: Json, indent: Int = 2, ascii: Boolean = false): String = {
    val b = new java.lang.StringBuilder; write(v, b, indent, ascii); b.toString
  }

  /** what is still to write: a value at a depth, a piece of text, a newline at a depth */
  private sealed trait Todo
  private final case class Value(v: Json, depth: Int) extends Todo
  private final case class Raw(s: String) extends Todo
  private final case class Newline(depth: Int) extends Todo

  private def write(root: Json, b: java.lang.StringBuilder, indent: Int, ascii: Boolean): Unit = {
    var todo: List[Todo] = List(Value(root, 0))
    while (todo.nonEmpty) {
      val head = todo.head; todo = todo.tail
      head match {
        case Raw(s) => b.append(s)
        case Newline(depth) =>
          if (indent >= 0) { b.append('\n'); var i = 0; while (i < indent * depth) { b.append(' '); i += 1 } }
        case Value(v, depth) => v match {
            case Null => b.append("null")
            case Bool(x) => b.append(if (x) "true" else "false")
            case Num(n) => b.append(n.bigDecimal.toPlainString)
            case Str(s) => quote(s, b, ascii)
            case Arr(items) =>
              if (items.isEmpty) b.append("[]")
              else {
                b.append('[')
                val inner = items.iterator.zipWithIndex.flatMap { case (x, i) =>
                  (if (i > 0) List(Raw(",")) else Nil) ++ List(Newline(depth + 1), Value(x, depth + 1))
                }.toList
                todo = inner ++ List(Newline(depth), Raw("]")) ++ todo
              }
            case Obj(fields) =>
              if (fields.isEmpty) b.append("{}")
              else {
                b.append('{')
                val sep = if (indent >= 0) ": " else ":"
                val inner = fields.iterator.zipWithIndex.flatMap { case ((k, x), i) =>
                  val key = new java.lang.StringBuilder; quote(k, key, ascii)
                  (if (i > 0) List(Raw(",")) else Nil) ++ List(Newline(depth + 1), Raw(key.toString + sep),
                    Value(x, depth + 1))
                }.toList
                todo = inner ++ List(Newline(depth), Raw("}")) ++ todo
              }
          }
      }
    }
  }
  private def quote(s: String, b: java.lang.StringBuilder, ascii: Boolean): Unit = {
    b.append('"')
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      c match {
        case '"' => b.append("\\\"")
        case '\\' => b.append("\\\\")
        case '\n' => b.append("\\n")
        case '\r' => b.append("\\r")
        case '\t' => b.append("\\t")
        case '\b' => b.append("\\b")
        case '\f' => b.append("\\f")
        case c if c < 0x20 || (ascii && c > 0x7e) => b.append(f"\\u${c.toInt}%04x")
        case c => b.append(c)
      }
      i += 1
    }
    b.append('"')
  }
}
