package okay.effects.describe

import okay.effects.Effects
import okay.effects.json.Json
import okay.effects.shape.{Problem, Problems, Shape}

/**
 * AN EFFECT AS A SERVICE: its operations described as data — a name, the parameters' shape, the result's shape,
 * whether it writes, and `make`, the operation with the identity answer as `perform` makes it — and `split`, an
 * operation back into its description, its arguments and its answer. From the two every projection follows
 * mechanically: a service SERVES the effect (decode the parameters, perform `make(p)` under the real handler, encode
 * the result), and a REMOTE handler performs it elsewhere (encode the arguments, call, decode the result, answer).
 * Written by hand, one `Op` per operation, on both compilers; the program does not know which.
 */
final case class Op[F[_], P, X](name: String, params: Shape[P], result: Shape[X], make: P => F[X], doc: Option[String] = None, writes: Boolean = false)

/** an operation on its way out: which, with what, and how its answer is read back */
final case class Call[X](op: String, args: Json, answer: Json => Either[Problems, X])

abstract class Describe[F[_]](val name: String) {
  def operations: List[Op[F, _, _]]
  /** every operation is described: the one this is, its arguments encoded, its answer decoded */
  def split[X](op: F[X]): Call[X]
  protected final def call[P, Y, X](o: Op[F, P, Y], p: P, k: Y => X): Call[X] = Call(o.name, o.params.encode(p), j => o.result.decode(j).map(k))
  final def operation(n: String): Option[Op[F, _, _]] = operations.find(_.name == n)
}

/** the result of a remote call did not decode, or the call failed: the service and the program disagree — not a
 * problem a person can fix, so not `Problems` */
final case class RemoteFailure(op: String, problems: Problems) extends RuntimeException(s"$op: ${Problem.render(problems)}")

/** the words over a description: serving an operation by name, and performing an effect remotely */
class Domains[F <: Effects with Singleton](implicit implementation: F) {
  val fx: F = implementation
  import fx._

  /** one call served: the parameters decoded, the operation performed under the handler in the row, the result encoded */
  def serve[E[_], R[_]](d: Describe[E])(name: String, args: Json)(implicit in: In[E, R]): Either[Problems, Json] ! R =
    d.operation(name) match {
      case None => pure[Either[Problems, Json], R](Left(List(Problem(List("operation"), s"one of ${d.operations.map(_.name).mkString(", ")}"))))
      case Some(o) => served[E, R](o, args)
    }
  private def served[E[_], R[_]](o: Op[E, _, _], args: Json)(implicit in: In[E, R]): Either[Problems, Json] ! R = typed(o, args)
  private def typed[E[_], P, X, R[_]](o: Op[E, P, X], args: Json)(implicit in: In[E, R]): Either[Problems, Json] ! R = o.params.decode(args) match {
    case Left(ps) => pure[Either[Problems, Json], R](Left(ps))
    case Right(p) => perform[E].apply[R, X](o.make(p)).map(x => Right(o.result.encode(x)))
  }

  /** the effect performed elsewhere: each operation split, carried by `transport` (a program of the row outside — a
   * fetch, a loopback), its answer decoded; a result that does not decode is a `RemoteFailure` */
  def remote[E[_]](d: Describe[E]): Remote[E] = new Remote[E](d)
  final class Remote[E[_]](d: Describe[E]) {
    def apply[R[_], Rest[_], A](transport: (Op[E, _, _], Json) => Json ! Rest)(p: A ! R)(implicit sp: Split[E, R, Rest]): A ! Rest =
      handle[E, R, Rest, A, A](p, new Handler[E, Rest, A, A] {
        def ret(a: A) = pure(a)
        def op[X](op: E[X], k: X => A ! Rest) = {
          val c = d.split(op)
          val o = d.operation(c.op).getOrElse(throw RemoteFailure(c.op, List(Problem(Nil, "not among the operations described"))))
          transport(o, c.args).flatMap(j => c.answer(j) match {
            case Right(x) => k(x)
            case Left(ps) => throw RemoteFailure(c.op, ps)
          })
        }
      })
  }
}
object Domains {
  def apply()(implicit fx: Effects): Domains[fx.type] = { implicit val exact: fx.type = fx; new Domains[fx.type] }
}
