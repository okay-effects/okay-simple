package okay.simple

/**
 * AN EFFECT'S NAME, of its operations. An effect's operations are a GADT, `F[X]` an operation answered by an `X`; in
 * a row an effect is named by a proper type — Scala 2 has no type alias whose result is a type constructor — and the
 * name says its operations: `type Ops[X]`. So the type of an effect's operations is computed from its name, the same
 * wherever it is asked, and an operation and the handler it goes to are of one type (`Effects.Op`).
 */
trait Effect { type Ops[X] }

/** a name of the operations `F`: `sealed trait Choose extends Name[Choose.Op]` */
trait Name[F[_]] extends Effect { type Ops[X] = F[X] }

/** the effect `F` with its parameter `S`, a name: `State % Int`, its operations `State[Int, X]` */
final abstract class %[F[_, _], S] extends Name[({ type L[x] = F[S, x] })#L]
