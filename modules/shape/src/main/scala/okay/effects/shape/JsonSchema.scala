package okay.effects.shape

import okay.effects.json.Json
import Json.{Arr, Obj, Str}

/** JSON Schema (draft 2020-12) of an erased `Schema`: what an MCP tool's `inputSchema`, OpenAPI and most form
 * libraries read. A record's optional fields are their inner schema and not `required`; a secret field is marked
 * `"x-secret": true` and nothing more — the policy is the application's. `Ref`s point into `$defs` */
object JsonSchema {
  private val Defs = "$" + "defs"
  def of(s: Schema, defs: Map[String, Schema] = Map.empty): Json =
    if (defs.isEmpty) node(s)
    else add(node(s), Defs -> Obj(defs.iterator.map { case (n, d) => n -> node(d) }.toVector))

  def node(s: Schema): Json = s match {
    case Schema.Text(cs) => add(Obj.of("type" -> Str("string")), cs.flatMap {
      case Constraint.MinLength(n) => List("minLength" -> Json.num(n))
      case Constraint.MaxLength(n) => List("maxLength" -> Json.num(n))
      case Constraint.Pattern(regex, hint) => List("pattern" -> Str(regex), "x-hint" -> Str(hint))
    }: _*)
    case Schema.Integer(cs) => add(Obj.of("type" -> Str("integer")), numeric(cs): _*)
    case Schema.Number(cs) => add(Obj.of("type" -> Str("number")), numeric(cs): _*)
    case Schema.Bool => Obj.of("type" -> Str("boolean"))
    case Schema.Date => Obj.of("type" -> Str("string"), "format" -> Str("date"))
    case Schema.Choice(options) => Obj.of("type" -> Str("string"), "enum" -> Arr(options.iterator.map(Str(_)).toVector))
    case Schema.Optional(of) => Obj.of("anyOf" -> Arr(Vector(node(of), Obj.of("type" -> Str("null")))))
    case Schema.Many(of, min, max) => add(Obj.of("type" -> Str("array"), "items" -> node(of)),
      (if (min > 0) List("minItems" -> Json.num(min)) else Nil) ++ max.map(m => "maxItems" -> Json.num(m)).toList: _*)
    case Schema.Record(fields) => record(fields, Vector.empty)
    case Schema.Sum(cases) => Obj.of("oneOf" -> Arr(cases.map { case (name, cs) =>
      val tag = Schema.Field("type", Schema.Choice(List(name)), None, None, secret = false, None)
      stripDescribed(cs) match {
        case Schema.Record(fields) => record(fields, Vector(tag), described(cs))
        case _ => Obj.of("type" -> Str("object"), "properties" -> Obj.of("type" -> Obj.of("const" -> Str(name)), "value" -> node(cs)),
          "required" -> Arr(Vector(Str("type"), Str("value"))))
      }
    }))
    case Schema.Described(of, label, doc) => add(node(of), ("title" -> Str(label)) :: doc.map(d => "description" -> Str(d)).toList: _*)
    case Schema.Ref(name) => Obj.of("$" + "ref" -> Str("#/" + Defs + "/" + name))
    case Schema.Any => Obj.empty
  }

  private def record(fields: Vector[Schema.Field], before: Vector[Schema.Field], words: List[(String, Json)] = Nil): Json = {
    val all = before ++ fields
    val properties = all.map(f => f.name -> property(f))
    val required = all.filter(f => !isOptional(f.schema) && f.default.isEmpty).map(f => Str(f.name))
    add(Obj.of("type" -> Str("object"), "properties" -> Obj(properties)),
      (if (required.isEmpty) Nil else List("required" -> Arr(required))) ++ words: _*)
  }
  private def property(f: Schema.Field): Json = {
    val base = f.schema match {
      case Schema.Optional(of) => node(of)
      case other => node(other)
    }
    add(base, f.label.map(l => "title" -> Str(l)).toList ++ f.doc.map(d => "description" -> Str(d)).toList ++
      f.default.map(d => "default" -> d).toList ++ (if (f.secret) List("x-secret" -> Json.Bool(true)) else Nil): _*)
  }
  private def numeric(cs: List[Constraint.OnNumber]): List[(String, Json)] = cs.map {
    case Constraint.Min(n) => "minimum" -> Json.Num(n)
    case Constraint.Max(n) => "maximum" -> Json.Num(n)
  }
  private def isOptional(s: Schema): Boolean = s match {
    case Schema.Optional(_) => true
    case Schema.Described(of, _, _) => isOptional(of)
    case _ => false
  }
  private def stripDescribed(s: Schema): Schema = s match {
    case Schema.Described(of, _, _) => stripDescribed(of)
    case other => other
  }
  private def described(s: Schema): List[(String, Json)] = s match {
    case Schema.Described(_, label, doc) => ("title" -> Str(label)) :: doc.map(d => "description" -> Str(d)).toList
    case _ => Nil
  }
  private def add(v: Json, fields: (String, Json)*): Json = v match {
    case o: Obj => Obj(o.fields ++ fields)
    case other => other
  }
}
