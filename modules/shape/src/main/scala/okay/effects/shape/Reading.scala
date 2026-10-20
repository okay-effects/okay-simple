package okay.effects.shape

import okay.effects.Effects
import okay.effects.algebra.{Applicative, FreeAp, Validated, ~>}
import okay.effects.json.Json

/**
 * `read(shape)`: a shape read through the `Fields` effect, over any implementation `fx` — a program that asks for
 * each leaf at its path, descends into records, sums, lists and options, keeps EVERY problem (the fields of a record
 * read into a `Validated` of programs), and asks again (`Retry`) where a handler offers another answer. Two handlers:
 * `fromJson(j)`, answering from a value — `run(fromJson(j)(read(s)))` is `s.decode(j)` — and `answering(xs)`, a
 * script of answers in order, a person's stand-in for tests. The interpreter is a `ShapeAlgebra`: no match on nodes.
 */
class Reading[F <: Effects](implicit implementation: F) {
  val fx: F = implementation
  import fx._

  /** a reader: given the path and the shape to show, the program of the value or the problems */
  type Rd[R[_], A] = (List[String], Schema.Field) => Either[Problems, A] ! R
  /** what is asked at a path that is no record's field: the root, a list's element, a sum's value */
  private def anonymous(name: String, schema: Schema, secret: Boolean = false): Schema.Field = Schema.Field(name, schema, None, None, secret, None)
  private def problem[A](path: List[String], message: String): Either[Problems, A] = Left(List(Problem(path, message)))

  def read[P, R[_]](shape: Shape[P], path: List[String] = Nil)(implicit in: In[Fields, R]): Either[Problems, P] ! R =
    shape.fold[({ type L[A] = Rd[R, A] })#L](new Reader[R])(path, anonymous(path.lastOption.getOrElse(""), shape.erased))

  final class Reader[R[_]](implicit in: In[Fields, R]) extends ShapeAlgebra[({ type L[A] = Rd[R, A] })#L] {
    private def get(path: List[String], shown: Schema.Field): Option[Json] ! R = perform[Fields].apply[R, Option[Json]](Fields.Get[Option[Json]](path, shown, j => j))
    private def retry(path: List[String], shown: Schema.Field, ps: Problems): Option[Json] ! R = perform[Fields].apply[R, Option[Json]](Fields.Retry[Option[Json]](path, shown, ps, j => j))
    private def has(path: List[String], shown: Schema.Field): Fields.Presence ! R = perform[Fields].apply[R, Fields.Presence](Fields.Has[Fields.Presence](path, shown, b => b))
    private def size(path: List[String], shown: Schema.Field): Option[Int] ! R = perform[Fields].apply[R, Option[Int]](Fields.Size[Option[Int]](path, shown, n => n))
    private def which(path: List[String], cases: List[String]): Option[String] ! R = perform[Fields].apply[R, Option[String]](Fields.Which[Option[String]](path, cases, c => c))

    /** a value in one piece: asked for, decoded by its WHOLE shape (refinements and all), asked again on a problem
     * while the handler offers another answer */
    private def leaf[A](shape: Shape[A]): Rd[R, A] = (path, shown) => get(path, shown).flatMap(leafFrom(shape, path, shown, _))
    /** the leaf decoded from what was got, asked again on a problem while the handler offers */
    private def leafFrom[A](shape: Shape[A], path: List[String], shown: Schema.Field, got: Option[Json]): Either[Problems, A] ! R = {
      def attempt(oj: Option[Json]): Either[Problems, A] ! R = oj match {
        case None => shape.absent match {
          case Some(a) => pure[Either[Problems, A], R](Right(a))
          case None => again(List(Problem(path, "required")))
        }
        case Some(j) => shape.decodeAt(j, path.reverse) match {
          case Right(a) => pure[Either[Problems, A], R](Right(a))
          case Left(ps) => again(ps)
        }
      }
      def again(ps: Problems): Either[Problems, A] ! R = retry(path, shown, ps).flatMap {
        case Some(j) => attempt(Some(j))
        case None => pure[Either[Problems, A], R](Left(ps))
      }
      attempt(got)
    }

    def text(constraints: List[Constraint.OnText]): Rd[R, String] = leaf(Shape.Text(constraints))
    def integer(constraints: List[Constraint.OnNumber]): Rd[R, BigInt] = leaf(Shape.Integer(constraints))
    def number(constraints: List[Constraint.OnNumber]): Rd[R, BigDecimal] = leaf(Shape.Number(constraints))
    def bool: Rd[R, Boolean] = leaf(Shape.Bool)
    def date: Rd[R, Day] = leaf(Shape.Date)
    def choice[A](options: List[(String, A)]): Rd[R, A] = leaf(Shape.Choice(options))
    def loose(schema: Schema, defs: Map[String, Schema]): Rd[R, Json] = leaf(Shape.Loose(schema, defs))
    def any: Rd[R, Json] = leaf(Shape.AnyValue)
    def deferred[A](name: String, of: () => Rd[R, A]): Rd[R, A] = (path, shown) => of()(path, shown)

    /** a leaf refined or described reads as ONE piece, by its whole shape; a compound one is read below and the
     * refinement applied to the result, asked again at the compound's path when a handler offers */
    def mapped[A, B](self: Shape.Mapped[A, B], of: Rd[R, A]): Rd[R, B] =
      if (self.isLeaf) leaf(self)
      else (path, shown) => of(path, shown).flatMap {
        case Left(ps) => pure[Either[Problems, B], R](Left(ps))
        case Right(a) => self.to(a) match {
          case Right(b) => pure[Either[Problems, B], R](Right(b))
          case Left(m) => pure[Either[Problems, B], R](Left(List(Problem(path, m))))
        }
      }
    def described[A](self: Shape.Described[A], of: Rd[R, A]): Rd[R, A] =
      if (self.isLeaf) leaf(self) else (path, shown) => of(path, shown.copy(schema = self.erased, label = shown.label.orElse(Some(self.label)), doc = shown.doc.orElse(self.doc)))

    def optional[A](self: Shape.Optional[A], of: Rd[R, A]): Rd[R, Option[A]] = (path, shown) =>
      has(path, shown).flatMap {
        case Fields.Presence.Absent => pure[Either[Problems, Option[A]], R](Right(None))
        case _ => of(path, shown.copy(schema = self.of.erased)).map(_.map(a => Some(a): Option[A]))
      }

    def many[A](self: Shape.Many[A], of: Rd[R, A]): Rd[R, List[A]] = (path, shown) =>
      has(path, shown).flatMap {
        case Fields.Presence.Absent => pure[Either[Problems, List[A]], R](problem(path, "required"))
        case Fields.Presence.Other(kind) => pure[Either[Problems, List[A]], R](problem(path, s"expected a list, got $kind"))
        case Fields.Presence.Present => size(path, shown).flatMap {
        case None => pure[Either[Problems, List[A]], R](problem(path, "required"))
        case Some(n) =>
          val bounds =
            if (n < self.min) List(Problem(path, s"at least ${self.min} items"))
            else if (self.max.exists(n > _)) List(Problem(path, s"at most ${self.max.get} items"))
            else Nil
          val elements: Either[Problems, Vector[A]] ! R = (0 until n).foldLeft(pure[Either[Problems, Vector[A]], R](Right(Vector.empty))) { (acc, i) =>
            acc.flatMap(sofar => of(path :+ i.toString, anonymous(i.toString, self.of.erased, shown.secret)).map(e => (sofar, e) match {
              case (Right(as), Right(a)) => Right(as :+ a)
              case (Left(ps), Right(_)) => Left(ps)
              case (Right(_), Left(ps)) => Left(ps)
              case (Left(ps), Left(qs)) => Left(ps ++ qs)
            }))
          }
          elements.map(e => (bounds, e) match {
            case (Nil, Right(as)) => Right(as.toList)
            case (bs, Right(_)) => Left(bs)
            case (bs, Left(ps)) => Left(bs ++ ps)
          })
        }
      }

    /** a compound's `required` at its own path becomes the field's default or `absent`, where the field has one */
    private def orDefault[A](f: Shape.Field[A], at: List[String])(e: Either[Problems, A]): Either[Problems, A] = e match {
      case Left(List(Problem(p, "required"))) if p == at => f.default.orElse(f.shape.absent).toRight(List(Problem(at, "required")))
      case other => other
    }

    /** the fields, each read at its path, into a Validated of programs: every field's problems kept */
    def record[P](self: Shape.Record[P]): Rd[R, P] = (path, shown) => has(path, shown).flatMap {
      case Fields.Presence.Absent => pure[Either[Problems, P], R](problem(path, "required"))
      case Fields.Presence.Other(kind) => pure[Either[Problems, P], R](problem(path, s"expected a record, got $kind"))
      case Fields.Presence.Present =>
      type V[A] = Validated[Problems, A] ! R
      implicit val applicative: Applicative[V] = new Applicative[V] {
        def pure[A](a: A): V[A] = fx.pure[Validated[Problems, A], R](Validated.Valid(a))
        def app[A, B](f: V[A => B], a: V[A]): V[B] = fx.flatMap[Validated[Problems, A => B], Validated[Problems, B], R](f)(vf =>
          fx.map[Validated[Problems, A], Validated[Problems, B], R](a)(va => Validated.applicative[Problems](okay.effects.algebra.Monoid.list[Problem]).app(vf, va)))
      }
      self.fields.foldMap[V](new (Shape.Field ~> V) {
        def apply[A](f: Shape.Field[A]): V[A] = {
          val at = path :+ f.name
          val rd: Either[Problems, A] ! R =
            if (f.shape.isLeaf) {
              // a leaf's absence is the leaf's own: `Get` answers None, then the default, else `absent`, else `required`
              f.default match {
                case Some(d) => get(at, f.erased).flatMap {
                  case None => fx.pure[Either[Problems, A], R](Right(d))
                  case got => leafFrom(f.shape, at, f.erased, got)
                }
                case None => field(f)(at, f.erased)
              }
            } else field(f)(at, f.erased).map(orDefault(f, at))
          fx.map[Either[Problems, A], Validated[Problems, A], R](rd)(Validated.fromEither)
        }
      }).map(_.toEither)
    }

    def sum[A](self: Shape.Sum[A]): Rd[R, A] = (path, shown) => {
      val names = self.cases.map(_.name)
      has(path, shown).flatMap {
        case Fields.Presence.Absent => pure[Either[Problems, A], R](problem(path, "required"))
        case Fields.Presence.Other(kind) => pure[Either[Problems, A], R](problem(path, s"expected a record with a 'type', got $kind"))
        case Fields.Presence.Present => which(path, names).flatMap {
        case None => pure[Either[Problems, A], R](problem(path, s"a 'type' field naming one of ${names.mkString(", ")}"))
        case Some(t) => self.cases.find(_.name == t) match {
          case None => pure[Either[Problems, A], R](problem(path :+ "type", s"one of ${names.mkString(", ")}"))
          case Some(c) =>
            val at = if (self.flat(c)) path else path :+ "value"
            c.shape.fold[({ type L[X] = Rd[R, X] })#L](this)(at, anonymous(c.name, c.shape.erased)).map(_.map(c.inject))
        }
        }
      }
    }
  }

  // ---- handlers
  /** the fields from a value: `Get` the value at the path (a field by name, an element by index), `Has` whether it is
   * there and not null, `Size` a list's length, `Which` the `type` field, `Retry` nothing — a wire has no second try */
  def fromJson(j: Json): FromJson = new FromJson(j)
  final class FromJson(j: Json) {
    private def at(path: List[String]): Option[Json] = path.foldLeft(Option(j)) {
      case (Some(o: Json.Obj), name) => o.get(name)
      case (Some(a: Json.Arr), index) => index.toIntOption.filter(i => i >= 0 && i < a.items.size).map(a.items)
      case _ => None
    }
    /** of the kind the shape asks: an object for a record or a sum, an array for a list; anything for the rest */
    private def compatible(shape: Schema, v: Json): Boolean = shape match {
      case Schema.Record(_) | Schema.Sum(_) => v.isInstanceOf[Json.Obj]
      case Schema.Many(_, _, _) => v.isInstanceOf[Json.Arr]
      case Schema.Described(of, _, _) => compatible(of, v)
      case _ => true
    }
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Fields, R, Rest]): A ! Rest =
      handle[Fields, R, Rest, A, A](p, new TailHandler[Fields, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Fields[X]): X = op match {
          case Fields.Get(path, _, answer) => answer(at(path))
          case Fields.Retry(_, _, _, answer) => answer(None)
          case Fields.Has(path, field, answer) => answer(at(path) match {
            case None | Some(Json.Null) => Fields.Presence.Absent
            case Some(v) => if (compatible(field.schema, v)) Fields.Presence.Present else Fields.Presence.Other(Json.kind(v))
          })
          case Fields.Size(path, _, answer) => answer(at(path).collect { case Json.Arr(items) => items.size })
          case Fields.Which(path, _, answer) => answer(at(path).collect { case o: Json.Obj => o.get("type") }.flatten.collect { case Json.Str(t) => t })
        }
      })
  }

  /** a script of answers, in the order the reader asks: `Get` and `Retry` take the next JSON (a `Retry` given `Null`,
   * or none left, gives up), `Has` a boolean, `Size` a number, `Which` a string — a person at a terminal, for a test */
  def answering(answers: List[Json]): Answering = new Answering(answers)
  final class Answering(answers: List[Json]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Fields, R, Rest]): A ! Rest =
      handle[Fields, List[Json], R, Rest, A, A](p, answers, new StatefulTailHandler[Fields, List[Json], Rest, A, A] {
        def ret(s: List[Json], a: A) = pure(a)
        private def next(cell: Cell[List[Json]]): Option[Json] = cell.get match {
          case j :: rest => cell.set(rest); Some(j)
          case Nil => None
        }
        def answer[X](cell: Cell[List[Json]], op: Fields[X]): X = op match {
          case Fields.Get(_, _, answer) => answer(next(cell) match { case Some(Json.Null) => None; case other => other })         // nothing entered
          case Fields.Retry(_, _, _, answer) => answer(next(cell) match { case Some(Json.Null) => None; case other => other })   // nothing entered: gives up
          case Fields.Has(_, _, answer) => answer(next(cell) match {
            case Some(Json.Bool(false)) | Some(Json.Null) | None => Fields.Presence.Absent
            case Some(_) => Fields.Presence.Present
          })
          case Fields.Size(_, _, answer) => answer(next(cell).collect { case Json.Num(n) => n.toInt })
          case Fields.Which(_, _, answer) => answer(next(cell).collect { case Json.Str(t) => t })
        }
      })
  }
}

object Reading {
  def apply()(implicit fx: Effects): Reading[fx.type] = {
    implicit val exact: fx.type = fx
    new Reading[fx.type]
  }
}
