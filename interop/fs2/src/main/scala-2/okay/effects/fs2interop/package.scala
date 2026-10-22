package okay.effects

/** `import okay.effects.fs2interop._`: the module on the machine, its words at once, nothing else to do */
package object fs2interop
  extends okay.effects.fs2interop.Fs2Streams[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](
    okay.effects.stream.`package`) {}
