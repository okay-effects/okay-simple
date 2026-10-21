package okay.effects

import okay.effects.Self

package object machine {
  /** THE MACHINE, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.machine.Machine._` gives the types and primitives, `import okay.effects.machine.Machine.std._`
   * the standard effects */
  val Machine: Runner with Self = new Engine with Self
  /** the machine as the implicit implementation alone, without its words: `import okay.effects.machine.implicits._`;
   * `import Machine._` gives both, by `Machine.self` */
  object implicits {
    implicit val machine: Machine.type = Machine
  }
}
