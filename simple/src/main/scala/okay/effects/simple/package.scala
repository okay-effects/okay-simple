package okay.effects

package object simple {
  /** THE MACHINE, with the standard effects, seen through the interface only: `import okay.effects.simple.Machine._`
   * gives the types and primitives, the effects and their handlers — the machine's own types abstract to its users */
  val Machine: Std = new machine.Machine with Std
}
