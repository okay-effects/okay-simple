package okay.effects.yaml

import okay.effects.json.Json

/**
 * YAML TEXT TO A VALUE, depending on nothing but the JSON value: a hand-written recursive descent over the text's
 * lines and their indentation, as `JsonText` is over its characters. What a configuration is written in — docker
 * compose, Kubernetes, CI files: block mappings and sequences, flow `[…]` and `{…}`, plain, single- and double-quoted
 * scalars, literal `|` and folded `>` blocks (chomping `-` `+`, an indentation digit), comments, anchors `&a` and aliases
 * `*a`, the merge key `<<`, tags (`!!str` kept, the rest read past), several documents (`---`, `...`). Scalars resolved
 * by YAML 1.2's core schema (null, booleans, integers in decimal, octal `0o` and hex `0x`, floats; `.inf` and `.nan`
 * kept as text: JSON has no such numbers). Not read: complex keys (`? `), and a key spanning lines. A failure says
 * where (the offset into the text) and what was expected there; `maxDepth` bounds the nesting, and so the stack.
 */
object YamlText {
  /** where a text stopped being YAML, and what was expected there */
  final case class Error(offset: Int, expected: String)

  /** a text of one document (none is null) */
  def parse(text: String, maxDepth: Int = 1000): Either[Error, Json] =
    parseAll(text, maxDepth).flatMap {
      case Vector() => Right(Json.Null)
      case Vector(one) => Right(one)
      case more => Left(Error(0, s"one document, not ${more.size} (parseAll reads them all)"))
    }

  /** a text of any number of documents, `---` between them */
  def parseAll(text: String, maxDepth: Int = 1000): Either[Error, Vector[Json]] = {
    val p = new Parser(text, maxDepth)
    try Right(p.documents()) catch { case Failed(at, what) => Left(Error(at, what)) }
  }

  private final case class Failed(at: Int, what: String) extends RuntimeException(what, null, false, false)

  /** a line of the text: where it starts, where its content starts (after the indentation), where it ends */
  private final class Line(val start: Int, var from: Int, val end: Int) {
    def indent: Int = from - start
  }

  private final class Parser(s: String, maxDepth: Int) {
    private val lines: Vector[Line] = {
      val b = Vector.newBuilder[Line]
      var i = 0
      while (i <= s.length) {
        val e0 = s.indexOf('\n', i)
        val e = if (e0 < 0) s.length else e0
        val end = if (e > i && s.charAt(e - 1) == '\r') e - 1 else e
        var f = i
        while (f < end && s.charAt(f) == ' ') f += 1
        b += new Line(i, f, end)
        i = e + 1
      }
      b.result()
    }
    var at = 0
    private val anchors = scala.collection.mutable.HashMap.empty[String, Json]

    def fail(offset: Int, what: String): Nothing = throw Failed(offset, what)
    private def line: Line = lines(at)
    private def done: Boolean = at >= lines.length
    private def char(i: Int): Int = if (i < s.length) s.charAt(i) else -1
    private def blank(l: Line): Boolean = l.from >= l.end || s.charAt(l.from) == '#'
    private def isMarker(l: Line, m: String): Boolean = l.indent == 0 && s.startsWith(m, l.from) &&
      (l.from + 3 == l.end || s.charAt(l.from + 3) == ' ' || s.charAt(l.from + 3) == '\t')
    private def skipBlank(): Unit = while (!done && blank(line)) at += 1
    private def deeper(depth: Int, where: Int): Unit = if (depth >= maxDepth) fail(where, s"nesting of at most $maxDepth")

    // ---- documents

    def documents(): Vector[Json] = {
      val out = Vector.newBuilder[Json]
      while (!done) {
        skipBlank()
        if (!done && s.charAt(line.from) == '%') { at += 1 } // a directive
        else if (!done && isMarker(line, "---")) {
          // `--- value` or `--- !tag`: what follows the marker is the document's first line
          val l = line
          val rest = l.from + 3
          var f = rest; while (f < l.end && s.charAt(f) == ' ') f += 1
          if (f < l.end && s.charAt(f) != '#') { l.from = f; out += document() }
          else {
            at += 1
            // a document begun: its content, or none — between two markers an empty one, at the end nothing
            if (nextIsContent) out += document() else if (!atEnd) out += Json.Null
          }
        } else if (!done && isMarker(line, "...")) { at += 1 }
        else if (!done) out += document()
      }
      out.result()
    }
    private def atEnd: Boolean = {
      var j = at
      while (j < lines.length && blank(lines(j))) j += 1
      j >= lines.length
    }
    private def nextIsContent: Boolean = {
      var j = at
      while (j < lines.length && blank(lines(j))) j += 1
      j < lines.length && !isMarker(lines(j), "---") && !isMarker(lines(j), "...")
    }
    private def document(): Json = {
      skipBlank()
      if (done || isMarker(line, "---") || isMarker(line, "...")) Json.Null
      else {
        val v = node(0, 0)
        skipBlank()
        if (!done && !isMarker(line, "---") && !isMarker(line, "...")) fail(line.from, "the end of the document")
        v
      }
    }

    // ---- block nodes

    /** a node whose first line is indented at least `min`: a sequence, a mapping, or a value */
    def node(min: Int, depth: Int): Json = {
      skipBlank()
      if (done) return Json.Null
      val l = line
      if (l.indent < min) return Json.Null
      deeper(depth, l.from)
      if (isDash(l.from, l.end)) sequence(l.indent, depth)
      else if (keyEnd(l.from, l.end) >= 0) mapping(l.indent, depth)
      else {
        // properties alone on a line, the node below them: `&a` / `!tag` then a block
        val (anchor, tag, rest) = properties(l.from, l.end)
        if (rest >= l.end || s.charAt(rest) == '#') {
          if (anchor.isEmpty && tag.isEmpty) fail(l.from, "a value")
          at += 1
          val v = node(min, depth + 1)
          anchor.foreach(a => anchors(a) = v)
          v
        } else if (s.charAt(rest) == '|' || s.charAt(rest) == '>') {
          val v = block(l, rest, l.indent - 1)
          anchor.foreach(a => anchors(a) = v)
          v
        } else {
          val v = onLine(l, rest, l.indent - 1, depth)
          val typed = if (tag.contains("!!str")) asText(v) else v
          anchor.foreach(a => anchors(a) = typed)
          typed
        }
      }
    }

    private def isDash(i: Int, end: Int): Boolean =
      char(i) == '-' && (i + 1 >= end || s.charAt(i + 1) == ' ' || s.charAt(i + 1) == '\t')

    /** where a block mapping's key ends on a line (its ':'), or -1: a ':' followed by a space or the end, outside
     * quotes and brackets */
    private def keyEnd(from: Int, end: Int): Int = {
      var i = from
      val c0 = char(i)
      if (c0 == '"' || c0 == '\'') {
        i = skipQuoted(i, end)
        while (i < end && s.charAt(i) == ' ') i += 1
        if (i < end && s.charAt(i) == ':' && (i + 1 >= end || s.charAt(i + 1) == ' ' || s.charAt(i + 1) == '\t')) i
        else -1
      } else if (c0 == '[' || c0 == '{' || c0 == '#' || c0 == '|' || c0 == '>' || c0 == '*' || c0 == '!' ||
        c0 == '&' && { val sp = s.indexOf(' ', i); sp < 0 || sp >= end || keyEnd(sp + 1, end) < 0 }) -1
      else {
        if (c0 == '&') { while (i < end && s.charAt(i) != ' ') i += 1; while (i < end && s.charAt(i) == ' ') i += 1 }
        while (i < end) {
          val c = s.charAt(i)
          if (c == ':' && (i + 1 >= end || s.charAt(i + 1) == ' ' || s.charAt(i + 1) == '\t')) return i
          if (c == '#' && i > from && s.charAt(i - 1) == ' ') return -1
          i += 1
        }
        -1
      }
    }

    private def skipQuoted(i: Int, end: Int): Int = {
      val q = s.charAt(i)
      var j = i + 1
      while (j < end) {
        val c = s.charAt(j)
        if (q == '\'' && c == '\'') { if (j + 1 < end && s.charAt(j + 1) == '\'') j += 2 else return j + 1 }
        else if (q == '"' && c == '\\') j += 2
        else if (q == '"' && c == '"') return j + 1
        else j += 1
      }
      end
    }

    def sequence(indent: Int, depth: Int): Json = {
      val items = Vector.newBuilder[Json]
      var going = true
      while (going) {
        skipBlank()
        if (done || line.indent != indent || !isDash(line.from, line.end) || isMarker(line, "---")) going = false
        else {
          val l = line
          var f = l.from + 1
          while (f < l.end && (s.charAt(f) == ' ' || s.charAt(f) == '\t')) f += 1
          if (f >= l.end || s.charAt(f) == '#') { at += 1; items += child(indent, depth) }
          else {
            // the rest of the line is the item, as if it began a line at its own column
            l.from = f
            items += node(l.indent, depth + 1)
          }
        }
      }
      Json.Arr(items.result())
    }

    /** a node below a key or a dash with nothing after it: more indented — or, under a key, a sequence at the key's
     * own indentation */
    private def child(indent: Int, depth: Int, underKey: Boolean = false): Json = {
      skipBlank()
      if (done) Json.Null
      else if (line.indent > indent) node(indent + 1, depth + 1)
      else if (underKey && line.indent == indent && isDash(line.from, line.end)) sequence(indent, depth + 1)
      else Json.Null
    }

    def mapping(indent: Int, depth: Int): Json = {
      val fields = scala.collection.mutable.LinkedHashMap.empty[String, Json]
      val merged = Vector.newBuilder[(String, Json)]
      var going = true
      while (going) {
        skipBlank()
        if (done || line.indent != indent || isMarker(line, "---") || isMarker(line, "...")) going = false
        else {
          val l = line
          val colon = keyEnd(l.from, l.end)
          if (colon < 0) {
            if (isDash(l.from, l.end)) fail(l.from, "a key (a sequence cannot follow a mapping at its indentation)")
            fail(l.from, "a key and ':'")
          }
          val (keyAnchor, _, keyAt) = properties(l.from, colon)
          val key = keyText(keyAt, colon)
          var f = colon + 1
          while (f < l.end && (s.charAt(f) == ' ' || s.charAt(f) == '\t')) f += 1
          val v =
            if (f >= l.end || s.charAt(f) == '#') { at += 1; child(indent, depth, underKey = true) }
            else {
              val (anchor, tag, rest) = properties(f, l.end)
              val value =
                if (rest >= l.end || s.charAt(rest) == '#') { at += 1; child(indent, depth, underKey = true) }
                else if (s.charAt(rest) == '|' || s.charAt(rest) == '>') block(l, rest, indent)
                else onLine(l, rest, indent, depth)
              val typed = if (tag.contains("!!str")) asText(value) else value
              anchor.foreach(a => anchors(a) = typed)
              typed
            }
          keyAnchor.foreach(a => anchors(a) = Json.Str(key))
          // the merge key: `<<` written plain (a quoted one is a key like any)
          val merge = key == "<<" && char(keyAt) != '"' && char(keyAt) != '\''
          if (merge) v match {
            case o: Json.Obj => merged ++= o.fields
            case Json.Arr(xs) => xs.foreach {
                case o: Json.Obj => merged ++= o.fields
                case _ => fail(l.from, "a mapping to merge")
              }
            case _ => fail(l.from, "a mapping (or mappings) to merge")
          }
          else fields(key) = v
        }
      }
      // what is merged gives way to what is written
      val extra = merged.result().filterNot(kv => fields.contains(kv._1))
      val seenExtra = scala.collection.mutable.LinkedHashMap.empty[String, Json]
      extra.foreach(kv => if (!seenExtra.contains(kv._1)) seenExtra(kv._1) = kv._2)
      Json.Obj((seenExtra.toVector ++ fields.toVector))
    }

    private def asText(j: Json): Json = j match {
      case Json.Str(_) => j
      case Json.Num(n) => Json.Str(n.toString)
      case Json.Bool(b) => Json.Str(b.toString)
      case Json.Null => Json.Str("")
      case other => other
    }

    private def keyText(from: Int, colon: Int): String = {
      var e = colon
      while (e > from && s.charAt(e - 1) == ' ') e -= 1
      val c = char(from)
      if (c == '"' || c == '\'') {
        val (v, after) = quoted(from, e)
        if (after != e) fail(after, "':' after the key")
        v
      } else s.substring(from, e)
    }

    /** an anchor `&a` and a tag `!t` before a value, in either order: them, and where the value starts */
    private def properties(from: Int, end: Int): (Option[String], Option[String], Int) = {
      var i = from
      var anchor = Option.empty[String]
      var tag = Option.empty[String]
      var going = true
      while (going && i < end) {
        val c = s.charAt(i)
        if ((c == '&' && anchor.isEmpty) || (c == '!' && tag.isEmpty)) {
          var j = i + 1
          while (j < end && s.charAt(j) != ' ' && s.charAt(j) != '\t') j += 1
          if (c == '&') anchor = Some(s.substring(i + 1, j)) else tag = Some(s.substring(i, j))
          i = j
          while (i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t')) i += 1
        } else going = false
      }
      (anchor, tag, i)
    }

    // ---- values on a line

    /** a value starting at `i` on line `l`: an alias, quoted, flow or plain (continued on more indented lines) — the
     * line (and any it took) consumed */
    private def onLine(l: Line, i: Int, parent: Int, depth: Int): Json = {
      val c = s.charAt(i)
      val (v, after) =
        if (c == '*') {
          var j = i + 1
          while (j < l.end && s.charAt(j) != ' ' && s.charAt(j) != ',' && s.charAt(j) != ']' && s.charAt(j) != '}') j += 1
          val name = s.substring(i + 1, j)
          (anchors.getOrElse(name, fail(i, s"an anchor &$name defined before")), j)
        } else if (c == '"' || c == '\'') {
          val (t, e) = quotedAcross(i)
          (Json.Str(t), e)
        } else if (c == '[' || c == '{') {
          val e = new Flow(i, depth).value()
          e
        } else {
          val (t, e) = plainAcross(l, i, parent)
          (resolve(t), e)
        }
      // what is left on its last line: a comment at most
      val last = lineOf(after)
      var j = after
      while (j < lines(last).end && (s.charAt(j) == ' ' || s.charAt(j) == '\t')) j += 1
      if (j < lines(last).end && s.charAt(j) != '#') fail(j, "the end of the line (or a comment)")
      at = last + 1
      v
    }

    private def lineOf(offset: Int): Int = {
      var j = at
      while (j + 1 < lines.length && lines(j + 1).start <= offset) j += 1
      j
    }

    /** a plain scalar from `i`: to a comment or the line's end; continued by more indented lines that are no key, no
     * dash — folded with a space */
    private def plainAcross(l: Line, i: Int, parent: Int): (String, Int) = {
      var e = l.end
      var k = i
      while (k < l.end) {
        if (s.charAt(k) == '#' && k > i && (s.charAt(k - 1) == ' ' || s.charAt(k - 1) == '\t')) { e = k; k = l.end }
        k += 1
      }
      var text = s.substring(i, e).trim
      var endAt = e
      var j = at + 1
      var going = true
      while (going && j < lines.length) {
        val n = lines(j)
        if (n.from >= n.end) { j += 1 }
        else if (n.indent > parent && s.charAt(n.from) != '#' && !isDash(n.from, n.end) && keyEnd(n.from, n.end) < 0 &&
          !isMarker(n, "---")) {
          text = text + " " + s.substring(n.from, n.end).trim
          endAt = n.end
          at = j
          j += 1
        } else going = false
      }
      (text, endAt)
    }

    private def quotedAcross(i: Int): (String, Int) = quoted(i, s.length)

    /** a quoted scalar from `i`: its text and where it ends — a line break inside folds to a space */
    private def quoted(i: Int, limit: Int): (String, Int) = {
      val q = s.charAt(i)
      val b = new java.lang.StringBuilder
      var j = i + 1
      while (j < limit) {
        val c = s.charAt(j)
        if (q == '\'' && c == '\'') {
          if (j + 1 < limit && s.charAt(j + 1) == '\'') { b.append('\''); j += 2 } else return (b.toString, j + 1)
        } else if (q == '"' && c == '"') return (b.toString, j + 1)
        else if (q == '"' && c == '\\') {
          if (j + 1 >= limit) fail(j, "an escape")
          s.charAt(j + 1) match {
            case 'n' => b.append('\n'); j += 2
            case 't' => b.append('\t'); j += 2
            case 'r' => b.append('\r'); j += 2
            case '0' => b.append('\u0000'); j += 2
            case '\\' => b.append('\\'); j += 2
            case '"' => b.append('"'); j += 2
            case '/' => b.append('/'); j += 2
            case ' ' => b.append(' '); j += 2
            case 'e' => b.append('\u001b'); j += 2
            case '\n' => j += 2; while (j < limit && (s.charAt(j) == ' ' || s.charAt(j) == '\t')) j += 1
            case 'x' => b.append(hex(j + 2, 2).toChar); j += 4
            case 'u' => b.append(hex(j + 2, 4).toChar); j += 6
            case 'U' => b.appendCodePoint(hex(j + 2, 8)); j += 10
            case _ => fail(j, "an escape: \\n \\t \\r \\0 \\\\ \\\" \\/ \\x.. \\u.... \\U........")
          }
        } else if (c == '\n' || c == '\r') {
          // a line break folds into a space; an empty line into a line break
          while (b.length > 0 && b.charAt(b.length - 1) == ' ') b.setLength(b.length - 1)
          var k = j
          var breaks = 0
          while (k < limit && (s.charAt(k) == '\n' || s.charAt(k) == '\r' || s.charAt(k) == ' ' || s.charAt(k) == '\t')) {
            if (s.charAt(k) == '\n') breaks += 1
            k += 1
          }
          if (breaks > 1) (1 until breaks).foreach(_ => b.append('\n')) else b.append(' ')
          j = k
        } else { b.append(c); j += 1 }
      }
      fail(i, s"the closing $q")
    }

    private def hex(from: Int, n: Int): Int = {
      if (from + n > s.length) fail(from, s"$n hex digits")
      var v = 0
      var k = 0
      while (k < n) {
        val d = Character.digit(s.charAt(from + k), 16)
        if (d < 0) fail(from + k, "a hex digit")
        v = v * 16 + d; k += 1
      }
      v
    }

    /** a literal `|` or folded `>` block: the header at `i` on `l`, its lines more indented than `parent` */
    private def block(l: Line, i: Int, parent: Int): Json = {
      val literal = s.charAt(i) == '|'
      var j = i + 1
      var chomp = 'c'
      var explicit = 0
      while (j < l.end && s.charAt(j) != ' ' && s.charAt(j) != '#') {
        s.charAt(j) match {
          case '-' => chomp = '-'
          case '+' => chomp = '+'
          case d if d >= '1' && d <= '9' => explicit = d - '0'
          case _ => fail(j, "a chomping indicator (- +) or an indentation digit")
        }
        j += 1
      }
      while (j < l.end && s.charAt(j) == ' ') j += 1
      if (j < l.end && s.charAt(j) != '#') fail(j, "the end of the block scalar's header")
      at += 1
      // its indentation: the given one, or the first non-empty line's
      var k = at
      while (k < lines.length && lines(k).from >= lines(k).end) k += 1
      val indent =
        if (explicit > 0) parent.max(0) + explicit
        else if (k < lines.length && lines(k).indent > parent) lines(k).indent
        else parent + 1
      val body = Vector.newBuilder[String]
      var going = true
      while (going && at < lines.length) {
        val n = lines(at)
        if (n.from >= n.end && n.end - n.start <= indent) { body += ""; at += 1 }
        else if (n.end - n.start >= indent && n.indent >= indent) { body += s.substring(n.start + indent, n.end); at += 1 }
        else going = false
      }
      val all = body.result()
      // the trailing empty lines: chomping decides
      val content = all.reverse.dropWhile(_.isEmpty).reverse
      val trailing = all.size - content.size
      val text =
        if (literal) content.mkString("\n")
        else {
          // FOLDED: a line break between two lines becomes a space; empty lines are line breaks; more indented lines
          // keep theirs
          val b = new StringBuilder
          var pending = 0
          var lastMore = false
          content.foreach { x =>
            if (x.isEmpty) pending += 1
            else {
              val more = x.startsWith(" ") || x.startsWith("\t")
              if (b.isEmpty) b ++= "\n" * pending
              else if (pending > 0) b ++= "\n" * (pending + (if (more || lastMore) 1 else 0))
              else b ++= (if (more || lastMore) "\n" else " ")
              b ++= x
              lastMore = more
              pending = 0
            }
          }
          b.toString
        }
      val end = chomp match {
        case '-' => ""
        case '+' => "\n" * (trailing + (if (content.nonEmpty) 1 else 0))
        case _ => if (content.nonEmpty) "\n" else ""
      }
      Json.Str(text + end)
    }

    /** CORE SCHEMA: what a plain scalar is */
    def resolve(t: String): Json = t match {
      case "" | "~" | "null" | "Null" | "NULL" => Json.Null
      case "true" | "True" | "TRUE" => Json.Bool(true)
      case "false" | "False" | "FALSE" => Json.Bool(false)
      case _ =>
        if (t.matches("""[-+]?[0-9]+""")) Json.Num(BigDecimal(t.stripPrefix("+")))
        else if (t.matches("""0o[0-7]+""")) Json.Num(BigDecimal(BigInt(t.drop(2), 8)))
        else if (t.matches("""0x[0-9a-fA-F]+""")) Json.Num(BigDecimal(BigInt(t.drop(2), 16)))
        else if (t.matches("""[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?""")) Json.Num(BigDecimal(t.stripPrefix("+")))
        else Json.Str(t)
    }

    // ---- flow collections

    /** `[…]` and `{…}`, over the text from `start` — across lines, comments skipped */
    private final class Flow(start: Int, depth0: Int) {
      var i = start
      def ws(): Unit = {
        var going = true
        while (going && i < s.length) {
          val c = s.charAt(i)
          if (c == ' ' || c == '\t' || c == '\n' || c == '\r') i += 1
          else if (c == '#' && (i == 0 || " \t\n".indexOf(s.charAt(i - 1)) >= 0)) {
            while (i < s.length && s.charAt(i) != '\n') i += 1
          } else going = false
        }
      }
      def value(): (Json, Int) = { val v = node(depth0); (v, i) }
      def node(depth: Int): Json = {
        ws()
        deeper(depth, i)
        val (anchor, tag, from) = properties(i, s.length)
        i = from
        val v: Json = char(i) match {
          case '[' => seq(depth)
          case '{' => map(depth)
          case '"' | '\'' => val (t, e) = quoted(i, s.length); i = e; Json.Str(t)
          case '*' =>
            val st = i + 1
            i += 1
            while (i < s.length && ",]}: \t\n\r".indexOf(s.charAt(i)) < 0) i += 1
            val name = s.substring(st, i)
            anchors.getOrElse(name, fail(st - 1, s"an anchor &$name defined before"))
          case -1 => fail(i, "a value")
          case _ => resolve(plain())
        }
        anchor.foreach(a => anchors(a) = v)
        if (tag.contains("!!str")) asText(v) else v
      }
      /** a plain scalar in a flow: to `,` `]` `}`, or a `:` before a space or one of them */
      def plain(): String = {
        val st = i
        var going = true
        while (going && i < s.length) {
          val c = s.charAt(i)
          if (c == ',' || c == ']' || c == '}' || c == '\n' || c == '\r') going = false
          else if (c == ':' && (i + 1 >= s.length || " \t\n\r,]}".indexOf(s.charAt(i + 1)) >= 0)) going = false
          else if (c == '#' && i > st && (s.charAt(i - 1) == ' ' || s.charAt(i - 1) == '\t')) going = false
          else i += 1
        }
        s.substring(st, i).trim
      }
      def seq(depth: Int): Json = {
        i += 1; ws()
        val items = Vector.newBuilder[Json]
        if (char(i) == ']') { i += 1; return Json.Arr(Vector.empty) }
        while (true) {
          val v = node(depth + 1)
          ws()
          // `[a: 1]`: a pair in a sequence is a one-field mapping
          val item = if (char(i) == ':') { i += 1; val w = node(depth + 1); Json.Obj(Vector(keyOf(v) -> w)) } else v
          items += item
          ws()
          char(i) match {
            case ',' => i += 1; ws(); if (char(i) == ']') { i += 1; return Json.Arr(items.result()) }
            case ']' => i += 1; return Json.Arr(items.result())
            case _ => fail(i, "',' or ']'")
          }
        }
        Json.Arr(items.result())
      }
      def map(depth: Int): Json = {
        i += 1; ws()
        val fields = scala.collection.mutable.LinkedHashMap.empty[String, Json]
        if (char(i) == '}') { i += 1; return Json.Obj(Vector.empty) }
        while (true) {
          ws()
          val k = keyOf(node(depth + 1))
          ws()
          val v = if (char(i) == ':') { i += 1; ws(); if (char(i) == ',' || char(i) == '}') Json.Null else node(depth + 1) }
            else Json.Null
          fields(k) = v
          ws()
          char(i) match {
            case ',' => i += 1; ws(); if (char(i) == '}') { i += 1; return Json.Obj(fields.toVector) }
            case '}' => i += 1; return Json.Obj(fields.toVector)
            case _ => fail(i, "',' or '}'")
          }
        }
        Json.Obj(fields.toVector)
      }
      def keyOf(j: Json): String = j match {
        case Json.Str(t) => t
        case Json.Null => ""
        case Json.Num(n) => n.toString
        case Json.Bool(b) => b.toString
        case _ => fail(i, "a scalar key")
      }
    }
  }

  // ---- THE OTHER WAY

  /** a value as block YAML, read back by `parse` to the same value; a text quoted where plain would read otherwise */
  def print(j: Json): String = {
    val b = new StringBuilder
    def scalar(v: Json): String = v match {
      case Json.Null => "null"
      case Json.Bool(x) => x.toString
      case Json.Num(n) => n.toString
      case Json.Str(t) => text(t)
      case Json.Arr(xs) if xs.isEmpty => "[]"
      case o: Json.Obj if o.fields.isEmpty => "{}"
      case _ => ""
    }
    def leaf(v: Json): Boolean = v match {
      case Json.Arr(xs) => xs.isEmpty
      case o: Json.Obj => o.fields.isEmpty
      case _ => true
    }
    def go(v: Json, indent: Int): Unit = v match {
      case o: Json.Obj if o.fields.nonEmpty =>
        o.fields.foreach { case (k, x) =>
          b ++= " " * indent ++= text(k) ++= ":"
          if (leaf(x)) b ++= " " ++= scalar(x) ++= "\n" else { b ++= "\n"; go(x, indent + 2) }
        }
      case Json.Arr(xs) if xs.nonEmpty =>
        xs.foreach { x =>
          b ++= " " * indent ++= "-"
          if (leaf(x)) b ++= " " ++= scalar(x) ++= "\n" else { b ++= "\n"; go(x, indent + 2) }
        }
      case x => b ++= " " * indent ++= scalar(x) ++= "\n"
    }
    go(j, 0)
    b.toString
  }

  /** a text plain when it reads back as itself, double-quoted (JSON's escapes) otherwise */
  def text(t: String): String = {
    val plainOk = t.nonEmpty && t.trim == t && !t.exists(c => c < ' ' || c == '\u007f') &&
      "-?:,[]{}#&*!|>'\"%@`".indexOf(t.charAt(0)) < 0 && !t.contains(": ") && !t.contains(" #") && !t.endsWith(":") &&
      (new Parser("", 1).resolve(t) match { case Json.Str(_) => true; case _ => false }) &&
      !t.startsWith(".") && t != "<<"
    if (plainOk) t else Json.print(Json.Str(t))
  }
}
