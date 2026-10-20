package okay.effects.shape

import okay.effects.algebra.FreeAp
import okay.effects.json.Json

/**
 * AN INTERPRETATION OF SHAPES: a method per kind of node, each given the node's parts with the children already
 * interpreted — `shape.fold(alg): G[P]`. The typed catamorphism over the shape tree, so an interpreter is written
 * with no match on nodes (Scala 2 has no GADT inference for one) and outside this module as easily as in it: the
 * reader (`Reading`), a form, a terminal's prompts. `self` is the node itself, erased, where an interpreter needs the
 * whole to show or to decode again.
 */
trait ShapeAlgebra[G[_]] {
  def text(constraints: List[Constraint.OnText]): G[String]
  def integer(constraints: List[Constraint.OnNumber]): G[BigInt]
  def number(constraints: List[Constraint.OnNumber]): G[BigDecimal]
  def bool: G[Boolean]
  def date: G[Day]
  def choice[A](options: List[(String, A)]): G[A]
  def optional[A](self: Shape.Optional[A], of: G[A]): G[Option[A]]
  def many[A](self: Shape.Many[A], of: G[A]): G[List[A]]
  /** a record: its fields, each a `Field` with a shape to `fold` with this algebra as needed */
  def record[P](self: Shape.Record[P]): G[P]
  def sum[A](self: Shape.Sum[A]): G[A]
  def mapped[A, B](self: Shape.Mapped[A, B], of: G[A]): G[B]
  def described[A](self: Shape.Described[A], of: G[A]): G[A]
  def loose(schema: Schema, defs: Map[String, Schema]): G[Json]
  def any: G[Json]
  /** by name, the interpretation of the shape given later: an interpreter must not force it before it is needed */
  def deferred[A](name: String, of: () => G[A]): G[A]

  /** a field's shape interpreted with this algebra */
  final def field[A](f: Shape.Field[A]): G[A] = f.shape.fold(this)
  /** a record's fields as a free applicative of their interpretations */
  final def fields[P](r: Shape.Record[P]): FreeAp[G, P] = r.fields.foldMap[({ type L[X] = FreeAp[G, X] })#L](new okay.effects.algebra.~>[Shape.Field, ({ type L[X] = FreeAp[G, X] })#L] {
    def apply[A](f: Shape.Field[A]): FreeAp[G, A] = FreeAp.lift[G, A](field(f))
  })(FreeAp.applicative[G])
}
