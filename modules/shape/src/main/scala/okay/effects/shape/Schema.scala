package okay.effects.shape

import okay.effects.json.Json

/** A SHAPE ERASED TO A VALUE: the fields, their kinds, constraints and words, with no decoder and no constructor. It
 * is what a file stores and a wire carries — a user-defined kind of record, an action's parameters to an agent — and
 * `Schema.loose` validates a `Json` against it with no typed result. `Schema.shape` is the meta-schema: a `Schema`
 * read and written by the same decoder as everything else */
sealed trait Schema
object Schema {
  final case class Text(constraints: List[Constraint.OnText]) extends Schema
  final case class Integer(constraints: List[Constraint.OnNumber]) extends Schema
  final case class Number(constraints: List[Constraint.OnNumber]) extends Schema
  case object Bool extends Schema
  case object Date extends Schema
  final case class Choice(options: List[String]) extends Schema
  final case class Optional(of: Schema) extends Schema
  final case class Many(of: Schema, min: Int, max: Option[Int]) extends Schema
  final case class Record(fields: Vector[Field]) extends Schema
  /** a tagged union: `{"type": <case name>, ...the case's fields}` when the case is a record, else
   * `{"type": <case name>, "value": <the case's value>}` */
  final case class OneOf(cases: Vector[(String, Schema)]) extends Schema
  final case class Described(of: Schema, label: String, doc: Option[String]) extends Schema
  /** a named schema, defined elsewhere (`defs`): how a recursive schema is written */
  final case class Ref(name: String) extends Schema
  /** any value at all */
  case object Any extends Schema
  final case class Field(name: String, schema: Schema, label: Option[String], doc: Option[String], secret: Boolean,
    default: Option[Json])

  def isRecord(s: Schema): Boolean = s match {
    case Record(_) => true
    case Described(of, _, _) => isRecord(of)
    case _ => false
  }
  /** whether an absent field of this schema is fine (it is `Optional`) */
  def isOptional(s: Schema, defs: Map[String, Schema]): Boolean = s match {
    case Optional(_) => true
    case Described(of, _, _) => isOptional(of, defs)
    case Ref(name) => defs.get(name).exists(isOptional(_, defs))
    case _ => false
  }

  /** a `Json` validated against the schema, normalised as the typed decoder would write it back */
  def loose(s: Schema, defs: Map[String, Schema] = Map.empty): Shape[Json] = Shape.Loose(s, defs)

  /** THE SCHEMA AS A SHAPE OF `Json`: the same tree — a record of fields, a sum of cases, a list, an option, the leaves
   * — each leaf's value kept as `Json`. So a schema read from a file (a user-defined kind of record) has a form, is
   * read field by field, is validated, exactly as a shape written in code is: `Loose` is this */
  def structural(s: Schema, defs: Map[String, Schema] = Map.empty): Shape[Json] = {
    import okay.effects.algebra.FreeAp
    def go(s: Schema): Shape[Json] = s match {
      case Text(cs) => Shape.Text(cs).map[Json](Json.Str(_)) { case Json.Str(x) => x; case other => Json.print(other) }
      case Integer(cs) => Shape.Integer(cs).map[Json](n => Json.Num(BigDecimal(n))) {
          case Json.Num(n) => n.toBigInt; case _ => BigInt(0)
        }
      case Number(cs) => Shape.Number(cs).map[Json](Json.Num(_)) { case Json.Num(n) => n; case _ => BigDecimal(0) }
      case Bool => Shape.Bool.map[Json](Json.Bool(_)) { case Json.Bool(b) => b; case _ => false }
      case Date => Shape.Date.map[Json](d => Json.Str(d.toString)) {
          case Json.Str(x) => Day.parse(x).getOrElse(Day(1970, 1, 1)); case _ => Day(1970, 1, 1)
        }
      case Choice(options) => Shape.Choice[Json](options.map(o => (o, Json.Str(o): Json)))
      case Optional(of) =>
        go(of).optional.map[Json](_.getOrElse(Json.Null)) { case Json.Null => None; case j => Some(j) }
      case Many(of, min, max) =>
        go(of).many(min, max).map[Json](l => Json.Arr(l.toVector)) { case Json.Arr(v) => v.toList; case _ => Nil }
      case Record(fields) =>
        val free = fields.foldLeft(FreeAp.pure[Shape.Field, Vector[(String, Json)]](Vector.empty)) { (acc, f) =>
          FreeAp.Ap[Shape.Field, Json, Vector[(String, Json)]](acc.map(v => (j: Json) => v :+ (f.name -> j)),
            FreeAp.lift[Shape.Field, Json](Shape.Field[Json](f.name, go(f.schema), f.label, f.doc, f.secret,
                f.default)))
        }
        Shape.Record[Json](free.map(Json.Obj(_)),
          {
            case o: Json.Obj => fields.flatMap(f => o.get(f.name).map(f.name -> _))
            case _ => Vector.empty
          })
      case OneOf(cases) => Shape.OneOf[Json](cases.toList.map { case (name, cs) =>
          val flat = isRecord(cs)
          Shape.Case[Json, Json](name, go(cs))(b =>
            if (flat) b match { case o: Json.Obj => ("type" -> Json.Str(name)) +: o; case other => other }
            else Json.Obj.of("type" -> Json.Str(name), "value" -> b))(a =>
            a match {
              case o: Json.Obj if o.get("type") == Some(Json.Str(name)) =>
                Some(if (flat) o.without("type") else o.get("value").getOrElse(Json.Null))
              case _ => None
            })
        })
      case Described(of, label, doc) => go(of).described(label, doc)
      case Ref(name) => defs.get(name) match {
          case Some(d) => Shape.deferred[Json](name)(go(d))
          case None => Shape.AnyValue.refine[Json](_ => Left(s"unknown schema '$name'"))(j => j)
        }
      case Any => Shape.AnyValue
    }
    go(s)
  }

  /** THE META-SCHEMA. Recursion through `Shape.deferred("schema")`, so `shape.erased` is finite: a `Ref("schema")`
   * where
   * a schema holds a schema, with `defs` giving it: `JsonSchema.of(Schema.shape.erased, Schema.defs)` */
  lazy val shape: Shape[Schema] = {
    import Shape.{any, bool, deferred, field, int, number, record, sub, sum, text, unit}
    val ref: Shape[Schema] = deferred("schema")(shape)
    val onText: Shape[Constraint.OnText] = sum(
      sub[Constraint.OnText, Constraint.MinLength]("min-length",
        record(field("n", int))(Constraint.MinLength(_))(_.n)) {
        case c: Constraint.MinLength => Some(c); case _ => None
      },
      sub[Constraint.OnText, Constraint.MaxLength]("max-length",
        record(field("n", int))(Constraint.MaxLength(_))(_.n)) {
        case c: Constraint.MaxLength => Some(c); case _ => None
      },
      sub[Constraint.OnText, Constraint.Pattern]("pattern",
        record(field("regex", text), field("hint", text))(Constraint.Pattern(_, _))(p => (p.regex, p.hint))) {
        case c: Constraint.Pattern => Some(c); case _ => None
      })
    val onNumber: Shape[Constraint.OnNumber] = sum(
      sub[Constraint.OnNumber, Constraint.Min]("min", record(field("n", number))(Constraint.Min(_))(_.n)) {
        case c: Constraint.Min => Some(c); case _ => None
      },
      sub[Constraint.OnNumber, Constraint.Max]("max", record(field("n", number))(Constraint.Max(_))(_.n)) {
        case c: Constraint.Max => Some(c); case _ => None
      })
    val fieldShape: Shape[Field] = record(
      field("name", text), field("schema", ref), field("label", text.optional), field("doc", text.optional),
      field("secret", bool).withDefault(false), field("default", any.optional))(Field(_, _, _, _, _, _))(f =>
      (f.name, f.schema, f.label, f.doc, f.secret, f.default))
    val caseShape: Shape[(String, Schema)] =
      record(field("name", text), field("schema", ref))((n: String, s: Schema) => (n, s))(c => c)
    sum(
      sub[Schema, Text]("text", record(field("constraints", onText.many()).withDefault(Nil))(Text(_))(_.constraints)) {
        case s: Text => Some(s); case _ => None
      },
      sub[Schema, Integer]("integer",
        record(field("constraints", onNumber.many()).withDefault(Nil))(Integer(_))(_.constraints)) {
        case s: Integer => Some(s); case _ => None
      },
      sub[Schema, Number]("number",
        record(field("constraints", onNumber.many()).withDefault(Nil))(Number(_))(_.constraints)) {
        case s: Number => Some(s); case _ => None
      },
      sub[Schema, Bool.type]("bool", unit.map(_ => Bool)(_ => ())) { case Bool => Some(Bool); case _ => None },
      sub[Schema, Date.type]("date", unit.map(_ => Date)(_ => ())) { case Date => Some(Date); case _ => None },
      sub[Schema, Choice]("choice", record(field("options", text.many()))(Choice(_))(_.options)) {
        case s: Choice => Some(s); case _ => None
      },
      sub[Schema, Optional]("optional", record(field("of", ref))(Optional(_))(_.of)) {
        case s: Optional => Some(s); case _ => None
      },
      sub[Schema, Many]("many",
        record(field("of", ref), field("min", int).withDefault(0), field("max", int.optional))(
          Many(_, _, _))(m => (m.of, m.min, m.max))) { case s: Many => Some(s); case _ => None },
      sub[Schema, Record]("record",
        record(field("fields", fieldShape.many()))(fs => Record(fs.toVector))(_.fields.toList)) {
        case s: Record => Some(s); case _ => None
      },
      sub[Schema, OneOf]("sum", record(field("cases", caseShape.many()))(cs => OneOf(cs.toVector))(_.cases.toList)) {
        case s: OneOf => Some(s); case _ => None
      },
      sub[Schema, Described]("described",
        record(field("of", ref), field("label", text), field("doc", text.optional))(
          Described(_, _, _))(d => (d.of, d.label, d.doc))) { case s: Described => Some(s); case _ => None },
      sub[Schema, Ref]("ref", record(field("name", text))(Ref(_))(_.name)) { case s: Ref => Some(s); case _ => None },
      sub[Schema, Any.type]("any", unit.map(_ => Any)(_ => ())) { case Any => Some(Any); case _ => None })
  }
  /** the definitions the meta-schema's erasure refers to */
  lazy val defs: Map[String, Schema] = Map("schema" -> shape.erased)
}
