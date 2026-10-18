package okay.effects.logic

/** `import okay.effects.logic.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Logic()(using okay.effects.machine.Machine)
export onMachine.*
