package okay.effects

/** `import okay.effects.yaml._`: the module on the machine, its words at once */
package object yaml extends okay.effects.yaml.Yamls[okay.effects.machine.Machine.type](okay.effects.machine.Machine) {
  /** a text of one document parsed, by recursive descent: the value, or where it failed and what was expected */
  def parse(text: String, maxDepth: Int = 1000): Either[okay.effects.parsing.ParseError, okay.effects.json.Json] =
    okay.effects.yaml.Descent.parse(text, maxDepth)
  /** a value as block YAML */
  def print(json: okay.effects.json.Json): String = okay.effects.yaml.YamlText.print(json)
}
