package okay.effects.ziointerop

/** `import okay.effects.ziointerop.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = ZioEffects()(using okay.effects.machine.Machine)
export onMachine.*

/** ZIO's own types as effects, on the machine */
val onMachineSteps = ZioSteps()(using okay.effects.machine.Machine)
export onMachineSteps.{Members, step, toZIOSteps, provideSteps, mapErrorSteps}
