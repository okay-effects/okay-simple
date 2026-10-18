package okay.effects.transaction

/** `import okay.effects.transaction.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Transactions()(using okay.effects.machine.Machine)
export onMachine.*
