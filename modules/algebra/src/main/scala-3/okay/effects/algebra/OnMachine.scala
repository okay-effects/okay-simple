package okay.effects.algebra

import okay.effects.machine.Machine

/** `import okay.effects.algebra.*`: the module on the machine — programs' instances `monad[R]`, `monadPlus[R]`,
 * `paraMonad[R]`, the words shaped for programs, the operators on them; and `import okay.effects.algebra.syntax.*`
 * the generic words and the operations on values of any instance */
val onMachine: Algebra[Machine.type] = Algebra()(using Machine)
export onMachine.{monad, monadPlus, paraMonad}
// forwarded by hand, not exported: an `export` of these into the package makes a cycle with the class's own imports
def traverse[A, B, R[_]](xs: Seq[A])(f: A => Machine.![B, R]): Machine.![Vector[B], R] = onMachine.traverse[A, B, R](xs)(f)
def sequence[A, R[_]](xs: Seq[Machine.![A, R]]): Machine.![Vector[A], R] = onMachine.sequence[A, R](xs)
def replicateA[A, R[_]](n: Int)(p: Machine.![A, R]): Machine.![Vector[A], R] = onMachine.replicateA[A, R](n)(p)
def loop[A, B, R[_]](a: A)(f: A => Machine.![Either[A, B], R]): Machine.![B, R] = onMachine.loop[A, B, R](a)(f)
implicit def programOps[A, R[_]](p: Machine.![A, R]): onMachine.ProgramOps[A, R] = new onMachine.ProgramOps[A, R](p)
/** the identity context: a bare value, `Monad[Id]` asked for by name */
type Id[A] = A
