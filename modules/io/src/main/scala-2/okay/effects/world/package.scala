package okay.effects

/** `import okay.effects.world._`: the module on the machine, its words at once, nothing else to do */
package object world extends okay.effects.world.Io[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {}
