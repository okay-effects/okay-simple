package okay.effects

package object machine {
  /** THE MACHINE, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.machine.Machine._` gives the types and primitives, `import okay.effects.machine.Machine.std._`
   * the standard effects */
  val Machine: Effects = new Engine
}
