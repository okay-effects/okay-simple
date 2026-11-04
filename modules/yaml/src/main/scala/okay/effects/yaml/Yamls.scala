package okay.effects.yaml

import okay.effects.json.Json
import okay.effects.parsing.ParseError

/** THE EFFECT: a YAML text to its value(s), the JSON value. How is the handler's choice — `descent`, the value
 * module's recursive descent over the lines, answered in place. A program that reads a configuration says only that
 * it reads one */
sealed trait YamlParser[X]
object YamlParser {
  final case class Parse[X](text: String, maxDepth: Int, answer: Either[ParseError, Json] => X) extends YamlParser[X]
  final case class ParseAll[X](text: String, maxDepth: Int, answer: Either[ParseError, Vector[Json]] => X)
    extends YamlParser[X]
}

/** YAML BY RECURSIVE DESCENT: the value module's `YamlText`, its failure as the parsing module's `ParseError` — the
 * offset, line and column, what was expected there */
object Descent {
  def parse(text: String, maxDepth: Int = 1000): Either[ParseError, Json] =
    YamlText.parse(text, maxDepth).left.map(e => ParseError.at(text, e.offset.toLong, List(e.expected)))
  def parseAll(text: String, maxDepth: Int = 1000): Either[ParseError, Vector[Json]] =
    YamlText.parseAll(text, maxDepth).left.map(e => ParseError.at(text, e.offset.toLong, List(e.expected)))
}

/** YAML: the effect `YamlParser` and its handler, over any implementation of okay-effects; the way out is
 * `YamlText.print` (a value as block YAML) */
class Yamls[I <: okay.effects.Effects with Singleton](val fx: I) {
  import fx._

  /** a text of one document parsed, by whichever handler is outside */
  def parseYaml[R[_]](text: String, maxDepth: Int = 1000)(implicit in: In[YamlParser, R]): Either[ParseError, Json] ! R =
    perform[YamlParser].apply[R, Either[ParseError, Json]](YamlParser.Parse[Either[ParseError, Json]](text, maxDepth,
      e => e))
  /** a text of documents parsed */
  def parseYamlAll[R[_]](text: String, maxDepth: Int = 1000)(
    implicit in: In[YamlParser, R]): Either[ParseError, Vector[Json]] ! R =
    perform[YamlParser].apply[R, Either[ParseError, Vector[Json]]](
      YamlParser.ParseAll[Either[ParseError, Vector[Json]]](text, maxDepth, e => e))

  /** by recursive descent, answered in place */
  def descent: DescentHandler = new DescentHandler
  final class DescentHandler {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[YamlParser, R, Rest]): A ! Rest =
      handle[YamlParser, R, Rest, A, A](p,
        new TailHandler[YamlParser, Rest, A, A] {
          def ret(a: A) = pure(a)
          def answer[X](op: YamlParser[X]): X = op match {
            case YamlParser.Parse(text, maxDepth, answer) => answer(Descent.parse(text, maxDepth))
            case YamlParser.ParseAll(text, maxDepth, answer) => answer(Descent.parseAll(text, maxDepth))
          }
        })
  }
}
