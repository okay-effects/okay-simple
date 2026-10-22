package okay.effects

/** `import okay.effects.probability._`: the module on the machine, its words at once, nothing else to do */
package object probability
  extends okay.effects.probability.Probability[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {}
