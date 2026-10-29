package okay.effects.json

import okay.effects.parsing.ParseError

/** JSON BY RECURSIVE DESCENT: the fast handler of `JsonParser`, 19× the grammar on effects (bench.d) — the value
 * module's `JsonText`, its failure as the grammar's error: the furthest point, line and column, and what was expected
 * there, so a test can hold the two to the same answers */
object Descent {
  def parse(text: String, maxDepth: Int = 1000): Either[ParseError, Json] =
    JsonText.parse(text, maxDepth).left.map(e => ParseError.at(text, e.offset.toLong, List(e.expected)))
}
