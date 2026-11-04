package okay.effects.yaml

/** `import okay.effects.yaml.*`: the module on the machine, its words at once */
val onMachine: Yamls[okay.effects.machine.Machine.type] = new Yamls(okay.effects.machine.Machine)
export onMachine.{parseYaml, parseYamlAll, descent}
/** a text of one document parsed, by recursive descent: the value, or where it failed and what was expected */
def parse(text: String, maxDepth: Int = 1000): Either[okay.effects.parsing.ParseError, okay.effects.json.Json] =
  Descent.parse(text, maxDepth)
/** a value as block YAML */
def print(json: okay.effects.json.Json): String = YamlText.print(json)
