package okay.effects

/** `import okay.effects.zstream._`: the module on the machine, its words at once, nothing else to do */
package object zstream
  extends okay.effects.zstream.ZioStreams[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](
    okay.effects.stream.`package`) {}
