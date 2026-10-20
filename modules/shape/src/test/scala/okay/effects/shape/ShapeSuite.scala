package okay.effects.shape

import okay.effects.json.Json

import Json.{Arr, Null, Num, Obj, Str, arr, num, obj}
import Shape._

class ShapeSuite extends munit.FunSuite {
  final case class Person(name: String, age: Int, email: Option[String], tags: List[String], born: Day)
  val person: Shape[Person] = record(
    field("name", text(Constraint.MinLength(1))).labelled("Name"),
    field("age", int).documented("in years"),
    field("email", text(Constraint.Pattern(".+@.+", "an email address")).optional),
    field("tags", text.many()).withDefault(Nil),
    field("born", date))(Person(_, _, _, _, _))(p => (p.name, p.age, p.email, p.tags, p.born))

  val ann: Person = Person("Ann", 41, Some("ann@x.io"), List("a"), Day(1985, 3, 9))
  val annValue: Json = obj("name" -> Str("Ann"), "age" -> num(41), "email" -> Str("ann@x.io"), "tags" -> arr(Str("a")), "born" -> Str("1985-03-09"))

  test("a record round trips") {
    assertEquals(person.encode(ann), annValue)
    assertEquals(person.decode(annValue), Right(ann))
  }
  test("absent optional is None, absent default is the default, absent required is a problem, extras are ignored") {
    val v = obj("name" -> Str("Bo"), "age" -> num(3), "born" -> Str("2023-01-31"), "extra" -> Json.Bool(true))
    assertEquals(person.decode(v), Right(Person("Bo", 3, None, Nil, Day(2023, 1, 31))))
    assertEquals(person.decode(obj("age" -> num(3))),
      Left(List(Problem(List("name"), "required"), Problem(List("born"), "required"))))
  }
  test("every field's problems, with paths, not the first") {
    val v = obj("name" -> Str(""), "age" -> Str("old"), "email" -> Str("nope"), "tags" -> arr(Str("ok"), num(2)), "born" -> Str("1985-13-01"))
    val problems = person.decode(v).left.getOrElse(Nil)
    assertEquals(problems.map(_.path), List(List("name"), List("age"), List("email"), List("tags", "1"), List("born")))
    assertEquals(problems.head.message, "at least 1 characters")
    assert(problems(1).message.startsWith("expected a whole number"))
    assertEquals(problems(2).message, "not an email address")
    assertEquals(problems(4).message, "not a date: 1985-13-1")
  }
  test("integer, number, choice, bool, many bounds") {
    assertEquals(integer.decode(Num(BigDecimal("2.5"))), Left(List(Problem(Nil, "expected a whole number"))))
    assertEquals(int.decode(num(1L << 40)), Left(List(Problem(Nil, "too large a number"))))
    assertEquals(number(Constraint.Min(0), Constraint.Max(10)).decode(num(11)), Left(List(Problem(Nil, "at most 10"))))
    sealed trait Color; case object Red extends Color; case object Blue extends Color
    val color = choice[Color]("red" -> Red, "blue" -> Blue)
    assertEquals(color.decode(Str("blue")), Right(Blue))
    assertEquals(color.encode(Red), Str("red"))
    assertEquals(color.decode(Str("green")), Left(List(Problem(Nil, "one of red, blue"))))
    assertEquals(Shape.bool.decode(num(1)), Left(List(Problem(Nil, "expected yes or no, got a number"))))
    assertEquals(int.many(min = 2).decode(arr(num(1))), Left(List(Problem(Nil, "at least 2 items"))))
  }
  test("mapped and checked") {
    val even = int.check("must be even")(_ % 2 == 0)
    assertEquals(even.decode(num(3)), Left(List(Problem(Nil, "must be even"))))
    assertEquals(even.decode(num(4)), Right(4))
    val upper = text.map(_.toUpperCase)(_.toLowerCase)
    assertEquals(upper.decode(Str("ab")), Right("AB"))
    assertEquals(upper.encode("AB"), Str("ab"))
  }
  test("a sum of records is flat, of other values has a value") {
    sealed trait Pet
    final case class Dog(name: String) extends Pet
    final case class Fish(litres: Int) extends Pet
    case object Rock extends Pet
    val pet: Shape[Pet] = sum(
      sub[Pet, Dog]("dog", record(field("name", text))(Dog(_))(_.name)) { case d: Dog => Some(d); case _ => None },
      sub[Pet, Fish]("fish", record(field("litres", int))(Fish(_))(_.litres)) { case f: Fish => Some(f); case _ => None },
      sub[Pet, Rock.type]("rock", unit.map(_ => Rock)(_ => ())) { case Rock => Some(Rock); case _ => None },
      Case[Pet, Int]("number", int)(n => Fish(n))(_ => None))
    assertEquals(pet.encode(Dog("Rex")), obj("type" -> Str("dog"), "name" -> Str("Rex")))
    assertEquals(pet.decode(obj("type" -> Str("dog"), "name" -> Str("Rex"))), Right(Dog("Rex")))
    assertEquals(pet.decode(obj("type" -> Str("rock"))), Right(Rock))
    assertEquals(pet.decode(obj("type" -> Str("number"), "value" -> num(3))), Right(Fish(3)))
    assertEquals(pet.decode(obj("type" -> Str("cat"))), Left(List(Problem(List("type"), "one of dog, fish, rock, number"))))
    assertEquals(pet.decode(obj("name" -> Str("x"))), Left(List(Problem(Nil, "a 'type' field naming one of dog, fish, rock, number"))))
    assertEquals(pet.decode(obj("type" -> Str("fish"), "litres" -> Str("many"))), Left(List(Problem(List("litres"), "expected a whole number, got text"))))
  }
  test("the erased schema says what the shape is") {
    val s = person.erased
    s match {
      case Schema.Record(fields) =>
        assertEquals(fields.map(_.name), Vector("name", "age", "email", "tags", "born"))
        assertEquals(fields(0).label, Some("Name"))
        assertEquals(fields(2).schema, Schema.Optional(Schema.Text(List(Constraint.Pattern(".+@.+", "an email address")))))
        assertEquals(fields(3).default, Some(arr()))
      case other => fail(s"not a record: $other")
    }
  }
  test("loose agrees with the typed decoder: typed decode then encode is loose decode") {
    val loose = Schema.loose(person.erased)
    val inputs = List(annValue,
      obj("name" -> Str("Bo"), "age" -> Num(BigDecimal("3.0")), "born" -> Str("2023-01-31"), "extra" -> Json.Bool(true)),
      obj("name" -> Str("Cy"), "age" -> num(9), "email" -> Null, "tags" -> arr(), "born" -> Str("2000-02-29")))
    inputs.foreach { v => assertEquals(loose.decode(v), person.decode(v).map(person.encode), v.toString) }
    val bad = obj("name" -> Str(""), "age" -> Str("old"), "born" -> Str("x"))
    assertEquals(loose.decode(bad), person.decode(bad).map(person.encode))
  }
  test("the meta-schema round trips a schema, and its own") {
    val s = person.erased
    assertEquals(Schema.shape.decode(Schema.shape.encode(s)), Right(s))
    val meta = Schema.shape.erased
    assertEquals(Schema.shape.decode(Schema.shape.encode(meta)), Right(meta))
    assertEquals(okay.effects.json.parse(Json.print(Schema.shape.encode(s))).toOption.flatMap(v => Schema.shape.decode(v).toOption), Some(s))
  }
  test("the meta-schema's text names the kinds") {
    val text = Json.print(Schema.shape.encode(Schema.Many(Schema.Optional(Schema.Date), 1, Some(3))))
    assertEquals(text, """{"type":"many","of":{"type":"optional","of":{"type":"date"}},"min":1,"max":3}""")
  }
  test("a deferred shape reads a tree") {
    final case class Tree(label: String, children: List[Tree])
    lazy val tree: Shape[Tree] = record(field("label", text), field("children", deferred("tree")(tree).many()).withDefault(Nil))(
      Tree(_, _))(t => (t.label, t.children))
    val t = Tree("a", List(Tree("b", Nil), Tree("c", List(Tree("d", Nil)))))
    assertEquals(tree.decode(tree.encode(t)), Right(t))
    assertEquals(Schema.loose(tree.erased, Map("tree" -> tree.erased)).decode(tree.encode(t)), Right(tree.encode(t)))
    assertEquals(Schema.loose(tree.erased).decode(tree.encode(t)), Left(List(Problem(List("children", "0"), "unknown schema 'tree'"), Problem(List("children", "1"), "unknown schema 'tree'"))))
  }
  test("a date") {
    assertEquals(Day.parse("2024-02-29"), Right(Day(2024, 2, 29)))
    assertEquals(Day.parse("2023-02-29"), Left("not a date: 2023-2-29"))
    assertEquals(Day.parse("2023-2-9"), Left("a date is yyyy-mm-dd, not '2023-2-9'"))
    assert(Day(2023, 1, 2) < Day(2023, 1, 10))
    assertEquals(Day(7, 1, 2).toString, "0007-01-02")
  }
}
