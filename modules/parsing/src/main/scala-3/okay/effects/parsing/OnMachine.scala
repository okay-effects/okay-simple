package okay.effects.parsing

/** `import okay.effects.parsing.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Parsing()(using okay.effects.machine.Machine)
export onMachine.*
