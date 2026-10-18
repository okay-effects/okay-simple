package okay.effects.jstream

/** `import okay.effects.jstream.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = new JavaStreams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export onMachine.*
