package okay.effects

/** `import okay.effects.resource._`: the module on the machine, its words at once, nothing else to do */
package object resource extends okay.effects.resource.Resources[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
