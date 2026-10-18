package okay.effects.probability

/** `import okay.effects.probability.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Probability()(using okay.effects.machine.Machine)
export onMachine.*
