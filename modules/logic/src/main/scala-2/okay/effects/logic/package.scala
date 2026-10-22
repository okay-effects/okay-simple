package okay.effects

/** `import okay.effects.logic._`: the module on the machine, its words at once, nothing else to do */
package object logic
  extends okay.effects.logic.Logic[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {}
