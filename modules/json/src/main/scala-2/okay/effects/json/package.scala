package okay.effects

/** `import okay.effects.json._`: the module on the machine, over the default streams, its words at once */
package object json extends okay.effects.json.Jsons[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](okay.effects.stream.`package`) {
  /** a text parsed on the machine: the value, or where it failed and what was expected */
  def parse(text: String, maxDepth: Int = 1000): Either[okay.effects.parsing.ParseError, okay.effects.json.Json] =
    okay.effects.machine.Machine.run(parser(text, maxDepth))
}
