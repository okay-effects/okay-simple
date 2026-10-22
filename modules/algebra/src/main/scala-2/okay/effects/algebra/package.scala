package okay.effects

/** `import okay.effects.algebra._`: the module on the machine — programs' instances `monad[R]`, `monadPlus[R]`,
 * `paraMonad[R]`; and `import okay.effects.algebra.syntax._` the generic words and the operations on values */
package object algebra
  extends okay.effects.algebra.Algebra[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
  /** the identity context: a bare value, `Monad[Id]` asked for by name */
  type Id[A] = A
}
