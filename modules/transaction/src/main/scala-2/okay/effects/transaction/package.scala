package okay.effects

/** `import okay.effects.transaction._`: the module on the machine, its words at once, nothing else to do */
package object transaction
  extends okay.effects.transaction.Transactions[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {}
