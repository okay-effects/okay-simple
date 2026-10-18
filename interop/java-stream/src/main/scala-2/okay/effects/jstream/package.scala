package okay.effects

/** `import okay.effects.jstream._`: the module on the machine, its words at once, nothing else to do */
package object jstream extends okay.effects.jstream.JavaStreams[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](okay.effects.stream.`package`) {
}
