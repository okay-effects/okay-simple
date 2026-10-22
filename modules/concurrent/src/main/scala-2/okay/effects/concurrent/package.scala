package okay.effects

/** `import okay.effects.concurrent._`: the module on the machine, its words at once, nothing else to do */
package object concurrent
  extends okay.effects.concurrent.Concurrent[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {}
