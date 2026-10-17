package okay.simple

/**
 * AN EFFECT'S NAME AND ITS OPERATIONS. An effect's operations are a GADT, `F[X]` an operation answered by an `X`;
 * in a row an effect is named by a proper type: Scala 2 has no type alias whose result is a type constructor, so
 * the name is what both compilers share. `Ops[E]` ties a name to its operations: one instance for every effect with
 * a parameter (`F % S`), one line of its own for an effect with none
 */
trait Ops[E] { type F[X] }

object Ops {
  type Aux[E, F0[_]] = Ops[E] { type F[X] = F0[X] }

  /** `F % S`'s operations: `F[S, X]` */
  implicit def parameterised[F0[_, _], S]: Aux[F0 % S, ({ type L[x] = F0[S, x] })#L] =
    new Ops[F0 % S] { type F[X] = F0[S, X] }

  /** the instance of an effect of no parameter: `implicit val ops: Ops.Aux[Choose, Choose.Op] = Ops.of` */
  def of[E, F0[_]]: Aux[E, F0] = new Ops[E] { type F[X] = F0[X] }
}

/** the effect `F` with its parameter `S`, a name: `State % Int`, its operations `State[Int, X]` */
final abstract class %[F[_, _], S]
