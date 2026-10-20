package okay.effects.shape

import okay.effects.json.Json
import Json.{Arr, Null, Num, Obj, Str}
import okay.effects.algebra.{Const, FreeAp, Monoid, Validated, ~>}

/**
 * A TYPED SCHEMA OF A VALUE `P`: at once a form for a person (fields, labels, constraints), an input schema for an
 * agent's tool, a request body, a command's flags — and THE DECODER that validates what any of them sends, with a
 * problem per field, all of them, not the first. Written once; every face derives from it. `erased` is the same
 * shape as a value, for files and wires; `Schema.loose` reads a value against an erased shape.
 *
 * Paths are kept innermost-first while decoding and reversed in a `Problem`.
 */
sealed trait Shape[P] {
  def decode(v: Json): Either[Problems, P] = decodeAt(v, Nil)
  def decodeAt(v: Json, path: List[String]): Either[Problems, P]
  def encode(p: P): Json
  def erased: Schema
  /** what an ABSENT field of this shape decodes to, if anything: `Optional` says `None`; the rest, nothing (required) */
  def absent: Option[P] = None
  /** this shape interpreted: the typed fold, a method of `alg` per node */
  def fold[G[_]](alg: ShapeAlgebra[G]): G[P]
  /** a leaf: a value in one piece — a primitive, or one refined or described; a record, a sum, a list, an option are not */
  def isLeaf: Boolean = true

  def map[B](to: P => B)(from: B => P): Shape[B] = Shape.Mapped(this, (p: P) => Right(to(p)), from)
  def refine[B](to: P => Either[String, B])(from: B => P): Shape[B] = Shape.Mapped(this, to, from)
  def check(message: String)(ok: P => Boolean): Shape[P] = Shape.Mapped(this, (p: P) => if (ok(p)) Right(p) else Left(message), (p: P) => p)
  def described(label: String, doc: Option[String] = None): Shape[P] = Shape.Described(this, label, doc)
  def optional: Shape[Option[P]] = Shape.Optional(this)
  def many(min: Int = 0, max: Option[Int] = None): Shape[List[P]] = Shape.Many(this, min, max)
}

object Shape extends RecordWords {
  private[shape] def problem[A](path: List[String], message: String): Either[Problems, A] = Left(List(Problem(path.reverse, message)))
  private[shape] def expected[A](what: String, v: Json, path: List[String]): Either[Problems, A] =
    problem(path, s"expected $what, got ${Json.kind(v)}")

  final case class Text(constraints: List[Constraint.OnText] = Nil) extends Shape[String] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, String] = v match {
      case Str(s) =>
        val ps = constraints.flatMap(_.check(s))
        if (ps.isEmpty) Right(s) else Left(ps.map(Problem(path.reverse, _)))
      case other => expected("text", other, path)
    }
    def encode(s: String): Json = Str(s)
    def erased: Schema = Schema.Text(constraints)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[String] = alg.text(constraints)
  }
  final case class Integer(constraints: List[Constraint.OnNumber] = Nil) extends Shape[BigInt] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, BigInt] = v match {
      case Num(n) if n.isWhole =>
        val ps = constraints.flatMap(_.check(n))
        if (ps.isEmpty) Right(n.toBigInt) else Left(ps.map(Problem(path.reverse, _)))
      case Num(_) => problem(path, "expected a whole number")
      case other => expected("a whole number", other, path)
    }
    def encode(n: BigInt): Json = Num(BigDecimal(n))
    def erased: Schema = Schema.Integer(constraints)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[BigInt] = alg.integer(constraints)
  }
  final case class Number(constraints: List[Constraint.OnNumber] = Nil) extends Shape[BigDecimal] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, BigDecimal] = v match {
      case Num(n) =>
        val ps = constraints.flatMap(_.check(n))
        if (ps.isEmpty) Right(n) else Left(ps.map(Problem(path.reverse, _)))
      case other => expected("a number", other, path)
    }
    def encode(n: BigDecimal): Json = Num(n)
    def erased: Schema = Schema.Number(constraints)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[BigDecimal] = alg.number(constraints)
  }
  case object Bool extends Shape[Boolean] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, Boolean] = v match {
      case Json.Bool(b) => Right(b)
      case other => expected("yes or no", other, path)
    }
    def encode(b: Boolean): Json = Json.Bool(b)
    def erased: Schema = Schema.Bool
    def fold[G[_]](alg: ShapeAlgebra[G]): G[Boolean] = alg.bool
  }
  case object Date extends Shape[Day] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, Day] = v match {
      case Str(s) => Day.parse(s).left.map(m => List(Problem(path.reverse, m)))
      case other => expected("a date", other, path)
    }
    def encode(d: Day): Json = Str(d.toString)
    def erased: Schema = Schema.Date
    def fold[G[_]](alg: ShapeAlgebra[G]): G[Day] = alg.date
  }
  /** one of a closed list, by name */
  final case class Choice[A](options: List[(String, A)]) extends Shape[A] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, A] = v match {
      case Str(s) => options.collectFirst { case (n, a) if n == s => a }
        .toRight(List(Problem(path.reverse, s"one of ${options.map(_._1).mkString(", ")}")))
      case other => expected(s"one of ${options.map(_._1).mkString(", ")}", other, path)
    }
    def encode(a: A): Json = options.collectFirst { case (n, x) if x == a => Str(n) }.getOrElse(Str(a.toString))
    def erased: Schema = Schema.Choice(options.map(_._1))
    def fold[G[_]](alg: ShapeAlgebra[G]): G[A] = alg.choice(options)
  }
  final case class Optional[A](of: Shape[A]) extends Shape[Option[A]] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, Option[A]] = v match {
      case Null => Right(None)
      case other => of.decodeAt(other, path).map(Some(_))
    }
    def encode(a: Option[A]): Json = a.fold[Json](Null)(of.encode)
    def erased: Schema = Schema.Optional(of.erased)
    override def absent: Option[Option[A]] = Some(None)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[Option[A]] = alg.optional(this, of.fold(alg))
    override def isLeaf: Boolean = false
  }
  final case class Many[A](of: Shape[A], min: Int = 0, max: Option[Int] = None) extends Shape[List[A]] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, List[A]] = v match {
      case Arr(items) =>
        val size =
          if (items.size < min) List(Problem(path.reverse, s"at least $min items"))
          else if (max.exists(items.size > _)) List(Problem(path.reverse, s"at most ${max.get} items"))
          else Nil
        val decoded = items.zipWithIndex.map { case (x, i) => of.decodeAt(x, i.toString :: path) }
        val problems = size ++ decoded.collect { case Left(ps) => ps }.flatten
        if (problems.isEmpty) Right(decoded.collect { case Right(a) => a }.toList) else Left(problems)
      case other => expected("a list", other, path)
    }
    def encode(as: List[A]): Json = Arr(as.iterator.map(of.encode).toVector)
    def erased: Schema = Schema.Many(of.erased, min, max)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[List[A]] = alg.many(this, of.fold(alg))
    override def isLeaf: Boolean = false
  }
  /** a record: `fields`, the free applicative of its `Field`s, decode it — run in `Validated`, every field's problems
   * kept — and list themselves without decoding (`erased`); `write` takes a value apart (`record(...)(make)(unmake)`
   * builds both) */
  final case class Record[P](fields: FreeAp[Field, P], write: P => Vector[(String, Json)]) extends Shape[P] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, P] = v match {
      case o: Obj => fields.foldMap[({ type L[A] = Validated[Problems, A] })#L](Field.reading(o, path))(Validated.applicative[Problems](Monoid.list[Problem])).toEither
      case other => expected("a record", other, path)
    }
    def encode(p: P): Json = Obj(write(p))
    def erased: Schema = Schema.Record(fields.analyze(Field.erasing)(Monoid.vector[Schema.Field]))
    def fold[G[_]](alg: ShapeAlgebra[G]): G[P] = alg.record(this)
    override def isLeaf: Boolean = false
  }
  /** a tagged union of cases: `{"type": name, ...}` — the case's own fields when it is a record, `"value"` otherwise */
  final case class OneOf[A](cases: List[Case[A]]) extends Shape[A] {
    private def names = cases.map(_.name).mkString(", ")
    def decodeAt(v: Json, path: List[String]): Either[Problems, A] = v match {
      case o: Obj => o.get("type") match {
        case Some(Str(t)) => cases.find(_.name == t) match {
          case Some(c) =>
            if (flat(c)) c.shape.decodeAt(o.without("type"), path).map(c.inject)
            else o.get("value") match {
              case Some(x) => c.shape.decodeAt(x, "value" :: path).map(c.inject)
              case None => c.shape.absent.map(c.inject).toRight(List(Problem(("value" :: path).reverse, "required")))
            }
          case None => problem("type" :: path, s"one of $names")
        }
        case Some(other) => expected(s"one of $names", other, "type" :: path)
        case None => problem(path, s"a 'type' field naming one of $names")
      }
      case other => expected("a record with a 'type'", other, path)
    }
    def encode(a: A): Json = cases.iterator.flatMap(c => c.write(a).map(v => (c, v))).nextOption() match {
      case Some((c, body)) =>
        body match {
          case o: Obj if flat(c) => ("type" -> Str(c.name)) +: o
          case other => Obj.of("type" -> Str(c.name), "value" -> other)
        }
      case None => Null
    }
    def erased: Schema = Schema.OneOf(cases.iterator.map(c => c.name -> c.shape.erased).toVector)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[A] = alg.sum(this)
    override def isLeaf: Boolean = false
    def flat(c: Case[A]): Boolean = Schema.isRecord(c.shape.erased)
  }
  sealed abstract class Case[A] {
    type B
    def name: String
    def shape: Shape[B]
    def inject: B => A
    def project: A => Option[B]
    final def write(a: A): Option[Json] = project(a).map(shape.encode)
  }
  object Case {
    def apply[A, B0](name0: String, shape0: Shape[B0])(inject0: B0 => A)(project0: A => Option[B0]): Case[A] = new Case[A] {
      type B = B0
      val name: String = name0
      val shape: Shape[B0] = shape0
      val inject: B0 => A = inject0
      val project: A => Option[B0] = project0
    }
  }
  /** a refinement or a conversion; `to` may say no, with a message — one problem at this path */
  final case class Mapped[A, B](of: Shape[A], to: A => Either[String, B], from: B => A) extends Shape[B] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, B] =
      of.decodeAt(v, path).flatMap(a => to(a).left.map(m => List(Problem(path.reverse, m))))
    def encode(b: B): Json = of.encode(from(b))
    def erased: Schema = of.erased
    override def absent: Option[B] = of.absent.flatMap(a => to(a).toOption)
    def fold[G[_]](alg: ShapeAlgebra[G]): G[B] = alg.mapped(this, of.fold(alg))
    override def isLeaf: Boolean = of.isLeaf
  }
  /** the same shape with words for a person: a face shows them, the decoder ignores them */
  final case class Described[A](of: Shape[A], label: String, doc: Option[String] = None) extends Shape[A] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, A] = of.decodeAt(v, path)
    def encode(a: A): Json = of.encode(a)
    def erased: Schema = Schema.Described(of.erased, label, doc)
    override def absent: Option[A] = of.absent
    def fold[G[_]](alg: ShapeAlgebra[G]): G[A] = alg.described(this, of.fold(alg))
    override def isLeaf: Boolean = of.isLeaf
  }
  /** a value validated against an erased schema, kept as a value — a user-defined kind of record, read from a file: the
   * schema's structural shape (`Schema.structural`), so it is read field by field, drawn as a form, validated, like a
   * shape written in code; `erased` is the schema itself */
  final case class Loose(schema: Schema, defs: Map[String, Schema] = Map.empty) extends Shape[Json] {
    lazy val structural: Shape[Json] = Schema.structural(schema, defs)
    def decodeAt(v: Json, path: List[String]): Either[Problems, Json] = structural.decodeAt(v, path)
    def encode(v: Json): Json = v
    def erased: Schema = schema
    override def absent: Option[Json] = structural.absent
    def fold[G[_]](alg: ShapeAlgebra[G]): G[Json] = structural.fold(alg)
    override def isLeaf: Boolean = structural.isLeaf
  }
  case object AnyValue extends Shape[Json] {
    def decodeAt(v: Json, path: List[String]): Either[Problems, Json] = Right(v)
    def encode(v: Json): Json = v
    def erased: Schema = Schema.Any
    def fold[G[_]](alg: ShapeAlgebra[G]): G[Json] = alg.any
  }
  /** a shape named and given later: how a recursive shape is written; erased to `Ref(name)` */
  final case class Deferred[A](name: String, of: () => Shape[A]) extends Shape[A] {
    lazy val inner: Shape[A] = of()
    def decodeAt(v: Json, path: List[String]): Either[Problems, A] = inner.decodeAt(v, path)
    def encode(a: A): Json = inner.encode(a)
    def erased: Schema = Schema.Ref(name)
    override def absent: Option[A] = inner.absent
    def fold[G[_]](alg: ShapeAlgebra[G]): G[A] = alg.deferred(name, () => inner.fold(alg))
    override def isLeaf: Boolean = inner.isLeaf
  }

  final case class Field[A](name: String, shape: Shape[A], label: Option[String] = None, doc: Option[String] = None,
                            secret: Boolean = false, default: Option[A] = None) {
    /** this field alone, as a record's fields */
    def free: FreeAp[Field, A] = FreeAp.lift[Field, A](this)
    def decodeAt(o: Obj, path: List[String]): Either[Problems, A] = o.get(name) match {
      case Some(v) => shape.decodeAt(v, name :: path)
      case None => default.orElse(shape.absent).toRight(List(Problem((name :: path).reverse, "required")))
    }
    def erased: Schema.Field = Schema.Field(name, shape.erased, label, doc, secret, default.map(shape.encode))
    def labelled(l: String): Field[A] = copy(label = Some(l))
    def documented(d: String): Field[A] = copy(doc = Some(d))
    def asSecret: Field[A] = copy(secret = true)
    def withDefault(a: A): Field[A] = copy(default = Some(a))
  }
  object Field {
    /** the fields read from an object at a path: the natural transformation `Record` runs its free applicative with */
    def reading(o: Obj, path: List[String]): Field ~> ({ type L[A] = Validated[Problems, A] })#L =
      new (Field ~> ({ type L[A] = Validated[Problems, A] })#L) {
        def apply[A](f: Field[A]): Validated[Problems, A] = Validated.fromEither(f.decodeAt(o, path))
      }
    /** the fields listed, nothing decoded */
    val erasing: Const[Field, Vector[Schema.Field]] = new Const[Field, Vector[Schema.Field]] {
      def apply[A](f: Field[A]): Vector[Schema.Field] = Vector(f.erased)
    }
  }

  // ---- the words
  def text: Shape[String] = Text()
  def text(constraints: Constraint.OnText*): Shape[String] = Text(constraints.toList)
  def integer: Shape[BigInt] = Integer()
  def integer(constraints: Constraint.OnNumber*): Shape[BigInt] = Integer(constraints.toList)
  def int: Shape[Int] = integer.refine(n => if (n.isValidInt) Right(n.toInt) else Left("too large a number"))(BigInt(_))
  def long: Shape[Long] = integer.refine(n => if (n.isValidLong) Right(n.toLong) else Left("too large a number"))(BigInt(_))
  def number: Shape[BigDecimal] = Number()
  def number(constraints: Constraint.OnNumber*): Shape[BigDecimal] = Number(constraints.toList)
  def bool: Shape[Boolean] = Bool
  def date: Shape[Day] = Date
  def any: Shape[Json] = AnyValue
  def choice[A](options: (String, A)*): Shape[A] = Choice(options.toList)
  def field[A](name: String, shape: Shape[A]): Field[A] = Field(name, shape)
  val unit: Shape[Unit] = Record[Unit](FreeAp.pure[Field, Unit](()), _ => Vector.empty)
  def sum[A](cases: Case[A]*): Shape[A] = OneOf(cases.toList)
  /** a case of a sum whose value IS a subtype: injected as it is */
  def sub[A, B <: A](name: String, shape: Shape[B])(project: A => Option[B]): Case[A] = Case[A, B](name, shape)(b => b)(project)
  def deferred[A](name: String)(of: => Shape[A]): Shape[A] = Deferred(name, () => of)
  def loose(schema: Schema, defs: Map[String, Schema] = Map.empty): Shape[Json] = Loose(schema, defs)
}
