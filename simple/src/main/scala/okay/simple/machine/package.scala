package okay.simple

import okay.std.Std

package object machine {
  /** THE MACHINE, with the standard effects, seen through the interface only: `import okay.simple.machine.Simple._`
   * gives the types and primitives, the effects and their handlers — the machine's own types abstract to its users */
  val Simple: Std = new Machine with Std
}
