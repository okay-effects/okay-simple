package okay.effects

/** `import okay.effects.resilience._`: the module on the machine, its words at once, nothing else to do */
package object resilience extends okay.effects.resilience.Resilience[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
