package okay.effects

/** `import okay.effects.stream._`: the module on the machine, its words at once, nothing else to do */
package object stream extends okay.effects.stream.Streams[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
