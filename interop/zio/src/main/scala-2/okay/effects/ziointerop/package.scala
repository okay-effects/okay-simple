package okay.effects

/** `import okay.effects.ziointerop._`: the module on the machine, its words at once, nothing else to do */
package object ziointerop extends okay.effects.ziointerop.ZioEffects[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
