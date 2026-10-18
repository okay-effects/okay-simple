package okay.effects.monixinterop

/** `import okay.effects.monixinterop.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = new Monix[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export onMachine.*
