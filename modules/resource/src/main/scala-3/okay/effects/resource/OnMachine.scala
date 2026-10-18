package okay.effects.resource

/** `import okay.effects.resource.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Resources()(using okay.effects.machine.Machine)
export onMachine.*
