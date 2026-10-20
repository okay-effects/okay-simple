package okay.effects.shape

import scala.compiletime.{constValueTuple, erasedValue, error, summonFrom}
import scala.deriving.Mirror

/** THE SHAPE OF A CASE CLASS OR A SEALED TRAIT, DERIVED — Scala 3, by `Mirror`, at compile time, on every platform.
 * A case class is a record of its fields, each field's shape found implicitly (`Shape.of`, an application's own) or
 * derived in turn; a sealed trait is a sum of its cases named `Shape.caseName`; a case object a record of no fields.
 * The value built is the one `record(...)` and `sum(...)` build by hand, so what is derived may be adjusted or
 * compared as any shape. Labels, docs, secrets, constraints are not in the type: say them afterwards */
trait ShapeDerivation:
  inline def derived[A](using m: Mirror.Of[A]): Shape[A] = inline m match
    case p: Mirror.ProductOf[A] =>
      val names = constValueTuple[p.MirroredElemLabels].productIterator.map(_.toString).toList
      val shapes = ShapeDerivation.shapesOf[p.MirroredElemTypes]
      Shape.fields[A](names.zip(shapes).map((n, s) => Shape.Field[Any](n, s.asInstanceOf[Shape[Any]])))(vs => p.fromProduct(Tuple.fromArray(vs.toArray)))(
        a => a.asInstanceOf[Product].productIterator.toList)
    case s: Mirror.SumOf[A] =>
      val names = constValueTuple[s.MirroredElemLabels].productIterator.map(n => Shape.caseName(n.toString)).toList
      val shapes = ShapeDerivation.shapesOf[s.MirroredElemTypes]
      Shape.OneOf[A](names.zip(shapes).zipWithIndex.map { case ((n, sh), i) =>
        Shape.Case[A, Any](n, sh.asInstanceOf[Shape[Any]])(b => b.asInstanceOf[A])(a => if s.ordinal(a) == i then Some(a) else None) })

  extension [P](shape: Shape[P])
    /** a field by name, checked against the case class `P` and typed as the field: `shape.at("tags").check("one at least")(_.nonEmpty)` */
    inline def at[N <: String & Singleton](name: N)(using m: Mirror.ProductOf[P]): Shape.At[P, ShapeDerivation.TypeOf[N, m.MirroredElemLabels, m.MirroredElemTypes]] =
      inline if ShapeDerivation.has[N, m.MirroredElemLabels] then new Shape.At(shape, name)
      else error("no field '" + scala.compiletime.constValue[N] + "' in the type")
    /** the same, the field selected: `shape.at(_.tags).check("one at least")(_.nonEmpty)` — the name read from the lambda, the type its own */
    transparent inline def at[T](inline sel: P => T): Shape.At[P, ?] = ${ Selectors.shapeAt[P, T]('shape, 'sel) }

object ShapeDerivation:
  /** the type of the field named `N` among labels `Ls` and types `Ts` */
  type TypeOf[N, Ls <: Tuple, Ts <: Tuple] = (Ls, Ts) match
    case (N *: _, t *: _) => t
    case (_ *: ls, _ *: ts) => TypeOf[N, ls, ts]
    case (EmptyTuple, EmptyTuple) => Nothing   // no such field: `at` says so, by name, before this is ever used
  inline def has[N, Ls <: Tuple]: Boolean = inline erasedValue[Ls] match
    case _: EmptyTuple => false
    case _: (N *: _) => true
    case _: (_ *: ls) => has[N, ls]
  inline def shapesOf[T <: Tuple]: List[Shape[?]] = inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => shapeOf[h] :: shapesOf[t]
  /** a field's shape: given; else an `Option`, a `List`, a `Vector` of a shape found the same way; else derived when
   * the type can be; else a compile error naming the type */
  inline def shapeOf[T]: Shape[T] = summonFrom {
    case s: Shape[T] => s
    case _ => inline erasedValue[T] match
      case _: Option[t] => Shape.Optional(shapeOf[t]).asInstanceOf[Shape[T]]
      case _: List[t] => Shape.Many(shapeOf[t]).asInstanceOf[Shape[T]]
      case _: Vector[t] => Shape.Many(shapeOf[t]).map(_.toVector)(_.toList).asInstanceOf[Shape[T]]
      case _ => summonFrom {
        case m: Mirror.Of[T] => Shape.derived[T](using m)
      }
  }
