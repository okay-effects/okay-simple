package okay.effects.world

/** `import okay.effects.world.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Io()(using okay.effects.machine.Machine)
export onMachine.*
