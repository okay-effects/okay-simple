package okay.effects.resilience

/** `import okay.effects.resilience.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Resilience()(using okay.effects.machine.Machine)
export onMachine.*
