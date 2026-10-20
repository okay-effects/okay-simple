package okay.effects.json

/** `import okay.effects.json.*`: the module on the machine, over the default streams, its words at once */
val onMachine: Jsons[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type] = new Jsons(okay.effects.stream.onMachine)
export onMachine.{value, parser, values, isSpace}
/** a text parsed on the machine: the value, or where it failed and what was expected */
def parse(text: String, maxDepth: Int = 1000): Either[okay.effects.parsing.ParseError, Json] = okay.effects.machine.Machine.run(onMachine.parser(text, maxDepth))
