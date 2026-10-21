package okay.effects

package object machine {
  /** THE MACHINE, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.machine.Machine._` gives the types and primitives, `import okay.effects.machine.Machine.std._`
   * the standard effects */
  val Machine: Runner = new Engine
  /** THE MACHINE AS THE IMPLICIT IMPLEMENTATION, opted into: `import okay.effects.machine.implicits._` and whatever asks
   * for an implicit `Machine.type`, `Runner` or `Effects` gets the machine — `new Faces()`, `run(program)`. Not implicit by
   * itself: code with an implementation of its own in scope would meet an ambiguity on Scala 2 */
  object implicits {
    implicit val machine: Machine.type = Machine
  }
}
