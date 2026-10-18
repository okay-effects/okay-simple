package okay.effects.tofuinterop

/** `import okay.effects.tofuinterop.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = new Tofu[okay.effects.machine.Machine.type](okay.effects.catsinterop.onMachine)
export onMachine.*
