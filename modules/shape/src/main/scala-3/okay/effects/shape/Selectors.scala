package okay.effects.shape

import scala.quoted.*

/** A FIELD SELECTED BY A LAMBDA, `_.field` or `_.a.b`, read from the lambda's tree at compile time: the names of the
 * case class fields selected, in order; anything else — a method, an expression — is refused by name */
object Selectors:
  def path[A: Type, B: Type](sel: Expr[A => B])(using Quotes): List[String] =
    import quotes.reflect.*
    def unwrap(t: Term): Term = t match
      case Inlined(_, _, inner) => unwrap(inner)
      case Block(Nil, inner) => unwrap(inner)
      case Typed(inner, _) => unwrap(inner)
      case other => other
    def select(t: Term, param: String): Option[List[String]] = unwrap(t) match
      case Ident(n) if n == param => Some(Nil)
      case s @ Select(q, n) if s.symbol.flags.is(Flags.CaseAccessor) => select(q, param).map(_ :+ n)
      case _ => None
    val found = unwrap(sel.asTerm) match
      case Block(List(DefDef(_, List(TermParamClause(List(p))), _, Some(body))), Closure(_, _)) => select(body, p.name).filter(_.nonEmpty)
      case _ => None
    found.getOrElse(report.errorAndAbort("a field is selected as _.field, or _.a.b, of a case class: not " + sel.show))

  /** the type of what the lambda's body selects, from the tree — the field's own, not what inference made of `B` */
  def typeOfSelected[A: Type, B: Type](sel: Expr[A => B])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    def unwrap(t: Term): Term = t match
      case Inlined(_, _, inner) => unwrap(inner)
      case Block(Nil, inner) => unwrap(inner)
      case Typed(inner, _) => unwrap(inner)
      case other => other
    unwrap(sel.asTerm) match
      case Block(List(DefDef(_, _, _, Some(body))), Closure(_, _)) => unwrap(body).tpe.widen
      case _ => TypeRepr.of[B]
  def shapeAt[P: Type, T: Type](shape: Expr[Shape[P]], sel: Expr[P => T])(using Quotes): Expr[Shape.At[P, ?]] =
    import quotes.reflect.*
    path(sel) match
      case List(name) => typeOfSelected(sel).asType match
        case '[t] => '{ new Shape.At[P, t]($shape, ${ Expr(name) }) }
      case names => report.errorAndAbort("a record's own field is adjusted: " + names.mkString(".") + " goes within")
