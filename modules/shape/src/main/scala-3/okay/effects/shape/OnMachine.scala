package okay.effects.shape

/** what a decoder says when it fails: never empty when returned */
type Problems = List[Problem]

/** `import okay.effects.shape.*`: the module on the machine — `read(shape)` through the `Fields` effect, and its
 * handlers `fromJson`, `answering` */
val onMachine: Reading[okay.effects.machine.Machine.type] = Reading()(using okay.effects.machine.Machine)
export onMachine.{read, fromJson, answering}
