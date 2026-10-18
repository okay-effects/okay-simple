package okay.effects.catsinterop

/** `import okay.effects.catsinterop.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Cats()(using okay.effects.machine.Machine)
export onMachine.*
