package okay.effects.zstream

/** `import okay.effects.zstream.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = new ZioStreams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export onMachine.*
