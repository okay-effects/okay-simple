package okay.effects.shape

import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}

/** THE SHAPE OF A CASE CLASS OR A SEALED TRAIT, DERIVED — Scala 2.13, by a blackbox macro, at compile time, on every
 * platform, the same value Scala 3 derives by `Mirror`: a case class a record of its fields, each field's shape found
 * implicitly or derived in turn; a sealed trait a sum of its cases named `Shape.caseName`; a case object a record of
 * no fields. The cases of a sealed trait must be known where `derived` is written (the same file, above it, as the
 * compiler sees them) */
trait ShapeDerivation {
  def derived[A]: Shape[A] = macro ShapeMacros.derived[A]
  /** `shape.at("name")`: a field by a literal name, checked against the case class `P` at compile time, typed as the field */
  implicit class AtWords[P](val shape: Shape[P]) {
    def at(name: String): Shape.At[P, _] = macro ShapeMacros.at[P]
  }
}

object ShapeMacros {
  def at[P: c.WeakTypeTag](c: whitebox.Context)(name: c.Tree): c.Tree = {
    import c.universe._
    val P = weakTypeOf[P]
    val n = name match { case Literal(Constant(s: String)) => s; case _ => c.abort(name.pos, "a field's name here is a literal, so the compiler can check it") }
    val sym = P.typeSymbol
    if (!sym.isClass || !sym.asClass.isCaseClass) c.abort(c.enclosingPosition, s"at(\"$n\") checks a field against a case class; $P is not one — use field(\"$n\")")
    val fields = P.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList
    fields.find(_.name.decodedName.toString == n) match {
      case Some(f) => val t = f.returnType.asSeenFrom(P, sym.asClass); q"new _root_.okay.effects.shape.Shape.At[$P, $t](${c.prefix.tree}.shape, $n)"
      case None => c.abort(name.pos, s"no field '$n' in $P: ${fields.map(_.name.decodedName.toString).mkString(", ")}")
    }
  }
  def derived[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[Shape[A]] = c.Expr[Shape[A]](shapeOf(c)(c.weakTypeOf[A], given = false))

  /** THE SHAPE OF A TYPE, AS A TREE, for another macro: given implicitly when `given`, else `Option`/`List`/`Vector`
   * of one, else derived — what `derived` is made of. `derived` itself does not look for a given shape of the type it
   * derives: `implicit val x: Shape[X] = Shape.derived[X]` would find itself */
  def shapeOf(c: blackbox.Context)(tpe: c.Type, given: Boolean = true): c.Tree = {
    import c.universe._
    val shapeOf = typeOf[Shape[_]].typeConstructor
    val option = typeOf[Option[_]].typeConstructor; val list = typeOf[List[_]].typeConstructor; val vector = typeOf[Vector[_]].typeConstructor
    def shapeFor(t: Type, seen: Set[Type]): Tree = {
      val given = c.inferImplicitValue(appliedType(shapeOf, t), silent = true)
      if (given.nonEmpty) given
      else if (t.typeConstructor =:= option) q"_root_.okay.effects.shape.Shape.Optional(${shapeFor(t.typeArgs.head, seen)})"
      else if (t.typeConstructor =:= list) q"_root_.okay.effects.shape.Shape.Many(${shapeFor(t.typeArgs.head, seen)})"
      else if (t.typeConstructor =:= vector) q"_root_.okay.effects.shape.Shape.Many(${shapeFor(t.typeArgs.head, seen)}).map(_.toVector)(_.toList)"
      else if (seen.exists(_ =:= t)) c.abort(c.enclosingPosition, s"a recursive shape for $t: write it with deferred, and give it implicitly")
      else derive(t, seen + t)
    }
    def derive(t: Type, seen: Set[Type]): Tree = {
      val sym = t.typeSymbol
      if (!sym.isClass) c.abort(c.enclosingPosition, s"no Shape for $t: give one, or make it a case class or a sealed trait")
      val cls = sym.asClass
      if (cls.isModuleClass) q"_root_.okay.effects.shape.Shape.fields[$t](_root_.scala.Nil)(_ => ${cls.module})(_ => _root_.scala.Nil)"
      else if (cls.isCaseClass) {
        val fields = t.decls.collect { case m: MethodSymbol if m.isCaseAccessor => m }.toList
        val fieldTrees = fields.map { f =>
          val ft = f.returnType.asSeenFrom(t, cls)
          q"_root_.okay.effects.shape.Shape.Field[_root_.scala.Any](${f.name.decodedName.toString}, ${shapeFor(ft, seen)}.asInstanceOf[_root_.okay.effects.shape.Shape[_root_.scala.Any]])"
        }
        val args = fields.zipWithIndex.map { case (f, i) => q"vs($i).asInstanceOf[${f.returnType.asSeenFrom(t, cls)}]" }
        val parts = fields.map(f => q"a.${f.name}")
        // `new`, not the companion's `apply`: a case class local to a method has no companion the macro can name
        q"""_root_.okay.effects.shape.Shape.fields[$t](_root_.scala.List(..$fieldTrees))((vs: _root_.scala.List[_root_.scala.Any]) => new $t(..$args))((a: $t) => _root_.scala.List[_root_.scala.Any](..$parts))"""
      } else if (cls.isSealed) {
        if (cls.fullName.startsWith("scala.") || cls.fullName.startsWith("java.")) c.abort(c.enclosingPosition, s"no Shape for $t, a library type: give one implicitly")
        cls.typeSignature   // the subclasses are known once the symbol is complete
        val subs = cls.knownDirectSubclasses.toList.sortBy(s => if (s.pos.isDefined) s.pos.point else 0)
        if (subs.isEmpty) c.abort(c.enclosingPosition, s"$t is sealed with no cases the compiler can see here: derive it below its cases, in their file")
        val cases = subs.map { sub =>
          val st = if (sub.isModuleClass) sub.asClass.module.typeSignature else sub.asType.toType
          val name = Shape.caseName(sub.name.decodedName.toString)
          q"""_root_.okay.effects.shape.Shape.Case[$t, $st]($name, ${shapeFor(st, seen)})((b: $st) => b)((a: $t) => a match { case b: $st => _root_.scala.Some(b); case _ => _root_.scala.None })"""
        }
        q"_root_.okay.effects.shape.Shape.OneOf[$t](_root_.scala.List(..$cases))"
      } else c.abort(c.enclosingPosition, s"no Shape for $t: give one, or make it a case class or a sealed trait")
    }
    if (given) shapeFor(tpe, Set.empty) else derive(tpe, Set(tpe))
  }
  /** a case class's fields, name and type, for another macro; none when not a case class */
  def fieldsOf(c: blackbox.Context)(tpe: c.Type): Option[List[(String, c.Type)]] = {
    import c.universe._
    val sym = tpe.typeSymbol
    if (sym.isClass && sym.asClass.isCaseClass) Some(tpe.decls.collect { case m: MethodSymbol if m.isCaseAccessor => (m.name.decodedName.toString, m.returnType.asSeenFrom(tpe, sym.asClass)) }.toList)
    else None
  }
}
