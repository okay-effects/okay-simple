package okay.effects

/** `import okay.effects.parsing._`: the module on the machine, its words at once, nothing else to do */
package object parsing extends okay.effects.parsing.Parsing[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
}
