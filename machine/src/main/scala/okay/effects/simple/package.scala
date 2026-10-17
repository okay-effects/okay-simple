package okay.effects

package object simple {
  /** THE MACHINE, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.simple.Machine._` gives the types and primitives, `import okay.effects.simple.Machine.std._`
   * the standard effects */
  val Machine: Effects = new machine.Machine
}
