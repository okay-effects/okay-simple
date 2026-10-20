package okay.effects.parsing

/** `import okay.effects.parsing.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Parsing()(using okay.effects.machine.Machine)
export onMachine.*
/** the input from a stream of chunks, on the default streams */
val fromStreams: StreamInput[okay.effects.stream.onMachine.type] = new StreamInput[okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export fromStreams.{inputFrom, parseStream}
