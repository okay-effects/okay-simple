package okay.effects.shape.checks

import okay.effects.json.Json
import okay.effects.json.Json.{Arr, Null, Num, Obj, Str, arr, num, obj}
import okay.effects.shape.{Constraint, Day, Fields, Problem, Problems, Reading, Schema, Shape, ShapeAlgebra}
import okay.effects.shape.Shape._
import okay.effects.{Runner, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class ReadingChecks(val fx0: Runner) extends munit.FunSuite {
  val rd: Reading[fx0.type] = new Reading[fx0.type]()(fx0)
  import fx0._
  type R[X] = Sum[Void, Fields, X]

  final case class Address(city: String, lines: List[String])
  final case class Person(name: String, age: Int, email: Option[String], tags: List[String], born: Day, home: Option[Address], note: String)
  val address: Shape[Address] = record(field("city", text(Constraint.MinLength(1))), field("lines", text.many(min = 1)))(Address(_, _))(a => (a.city, a.lines))
  val person: Shape[Person] = record(
    field("name", text(Constraint.MinLength(1))).labelled("Name"),
    field("age", int.check("of age")(_ >= 18)),
    field("email", text(Constraint.Pattern(".+@.+", "an email address")).optional),
    field("tags", text.many()).withDefault(Nil),
    field("born", date),
    field("home", address.optional),
    field("note", text).withDefault("-"))(Person(_, _, _, _, _, _, _))(p => (p.name, p.age, p.email, p.tags, p.born, p.home, p.note))

  sealed trait Pet
  final case class Dog(name: String) extends Pet
  final case class Fish(litres: Int) extends Pet
  val pet: Shape[Pet] = sum(
    sub[Pet, Dog]("dog", record(field("name", text))(Dog(_))(_.name)) { case d: Dog => Some(d); case _ => None },
    sub[Pet, Fish]("fish", record(field("litres", int))(Fish(_))(_.litres)) { case f: Fish => Some(f); case _ => None })

  def fromJson[A](s: Shape[A], j: Json): Either[Problems, A] = fx0.run(rd.fromJson(j)(rd.read[A, R](s)))
  def answering[A](s: Shape[A], answers: Json*): Either[Problems, A] = fx0.run(rd.answering(answers.toList)(rd.read[A, R](s)))

  val ann = obj("name" -> Str("Ann"), "age" -> num(41), "email" -> Str("ann@x.io"), "tags" -> arr(Str("a")), "born" -> Str("1985-03-09"),
    "home" -> obj("city" -> Str("Kyiv"), "lines" -> arr(Str("1 Main"))), "note" -> Str("hi"))

  test("the law: read from a json is decode, values and problems alike") {
    val inputs = List(ann,
      obj("name" -> Str("Bo"), "age" -> num(20), "born" -> Str("2000-02-29")),
      obj("name" -> Str(""), "age" -> num(3), "email" -> Str("nope"), "tags" -> arr(Str("ok"), num(2)), "born" -> Str("x"),
        "home" -> obj("city" -> Str(""), "lines" -> arr())),
      obj("age" -> Str("old")),
      obj("name" -> Str("Cy"), "age" -> num(30), "born" -> Str("2000-01-01"), "home" -> Null, "email" -> Null),
      obj("name" -> Str("Di"), "age" -> num(30), "born" -> Str("2000-01-01"), "home" -> Str("x"), "tags" -> Str("y")),
      Str("not a record"))
    inputs.foreach(j => assertEquals(fromJson(person, j), person.decode(j), Json.print(j)))
    List(obj("type" -> Str("dog"), "name" -> Str("Rex")), obj("type" -> Str("fish"), "litres" -> Str("many")), obj("type" -> Str("cat")), obj("name" -> Str("x")))
      .foreach(j => assertEquals(fromJson(pet, j), pet.decode(j), Json.print(j)))
    assertEquals(fromJson(int.many(min = 2), arr(num(1))), int.many(min = 2).decode(arr(num(1))))
    assertEquals(fromJson(Schema.shape, Schema.shape.encode(person.erased)), Right(person.erased))
  }
  test("a person's answers, in order, asked again on a problem") {
    val got = answering(person,
      Json.Bool(true),                  // a record here
      Str("Ann"),                       // name
      num(3), num(41),                  // age: not of age, asked again
      Json.Bool(true), Str("ann@x.io"), // email: present, the value
      Json.Bool(true), num(1), Str("a"),   // tags: present, one, the value
      Str("1985-13-09"), Str("1985-03-09"),  // born: not a date, asked again
      Json.Bool(true), Json.Bool(true), Str("Kyiv"), Json.Bool(true), num(1), Str("1 Main"),   // home: present, a record; city; lines: present, one
      Null)                             // note: nothing entered: the default
    assertEquals(got, Right(Person("Ann", 41, Some("ann@x.io"), List("a"), Day(1985, 3, 9), Some(Address("Kyiv", List("1 Main"))), "-")))
  }
  test("a person who gives up: the problems stand, every field's") {
    val got = answering(person, Json.Bool(true), Str(""), Null, num(3), Null, Json.Bool(false), Json.Bool(false), Str("2001-01-01"), Json.Bool(false), Str("n"))
    assertEquals(got, Left(List(Problem(List("name"), "at least 1 characters"), Problem(List("age"), "of age"))))
  }
  test("a sum asked which case") {
    assertEquals(answering(pet, Json.Bool(true), Str("fish"), Json.Bool(true), num(20)), Right(Fish(20)))
    assertEquals(answering(pet, Json.Bool(true), Str("bird")), Left(List(Problem(List("type"), "one of dog, fish"))))
    assertEquals(answering(pet, Json.Bool(false)), Left(List(Problem(Nil, "required"))))
  }
  test("an interpreter of one's own: the paths of the leaves, by the algebra, nothing decoded") {
    type Paths[A] = List[String] => List[String]
    val paths: ShapeAlgebra[Paths] = new ShapeAlgebra[Paths] {
      private def here: Paths[Nothing] = p => List(p.mkString("."))
      def text(constraints: List[Constraint.OnText]) = here
      def integer(constraints: List[Constraint.OnNumber]) = here
      def number(constraints: List[Constraint.OnNumber]) = here
      def bool = here
      def date = here
      def choice[A](options: List[(String, A)]) = here
      def loose(schema: Schema, defs: Map[String, Schema]) = here
      def any = here
      def optional[A](self: Shape.Optional[A], of: Paths[A]) = p => of(p).map(_ + "?")
      def many[A](self: Shape.Many[A], of: Paths[A]) = p => of(p :+ "*")
      def record[P](self: Shape.Record[P]) = p => self.erased match { case Schema.Record(fs) => fs.toList.flatMap(f => field(self.fields.analyze(new okay.effects.algebra.Const[Shape.Field, List[Shape.Field[_]]] { def apply[A](x: Shape.Field[A]) = List(x) })(okay.effects.algebra.Monoid.list).find(_.name == f.name).get)(p :+ f.name)); case _ => Nil }
      def sum[A](self: Shape.Sum[A]) = p => self.cases.flatMap(c => c.shape.fold(this)(if (self.flat(c)) p :+ c.name else p :+ c.name :+ "value"))
      def mapped[A, B](self: Shape.Mapped[A, B], of: Paths[A]) = of
      def described[A](self: Shape.Described[A], of: Paths[A]) = of
      def deferred[A](name: String, of: () => Paths[A]) = p => List((p :+ name).mkString("."))
    }
    assertEquals(person.fold(paths)(Nil), List("name", "age", "email?", "tags.*", "born", "home.city?", "home.lines.*?", "note"))
    assertEquals(pet.fold(paths)(Nil), List("dog.name", "fish.litres"))
  }
}

class ReadingOnMachine extends ReadingChecks(Machine)
class ReadingOnFree extends ReadingChecks(Free)
