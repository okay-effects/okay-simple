package okay.effects

/** `import okay.effects.catsinterop._`: the module on the machine, its words at once, nothing else to do */
package object catsinterop extends okay.effects.catsinterop.Cats[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
