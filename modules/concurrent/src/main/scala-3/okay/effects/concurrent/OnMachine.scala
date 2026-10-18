package okay.effects.concurrent

/** `import okay.effects.concurrent.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Concurrent()(using okay.effects.machine.Machine)
export onMachine.*
