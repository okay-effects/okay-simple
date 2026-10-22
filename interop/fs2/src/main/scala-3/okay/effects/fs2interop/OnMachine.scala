package okay.effects.fs2interop

/** `import okay.effects.fs2interop.*`: the module on the machine, its words at once, nothing else to do */
val onMachine =
  new Fs2Streams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)
export onMachine.*
