package okay.effects.shape

import okay.effects.json.Json
import okay.effects.shape.Shape.{date, field, int, record, text, unit}

final case class Address(city: String, lines: List[String])
final case class Person(id: String, name: String, born: Option[Day], vip: Boolean, score: Option[Int], address: Option[Address], tags: List[String])
sealed trait Pet
final case class Dog(name: String, age: Int) extends Pet
case object Fish extends Pet
final case class Nested(who: Person, pets: List[Pet])

/** a shape derived is the shape written by hand: the same encoding, the same decoding, the same erasure */
class DerivedSuite extends munit.FunSuite {
  val address: Shape[Address] = record(field("city", text), field("lines", text.many()))(Address(_, _))(a => (a.city, a.lines))
  val person: Shape[Person] = record(field("id", text), field("name", text), field("born", date.optional), field("vip", Shape.bool), field("score", int.optional),
    field("address", address.optional), field("tags", text.many()))(Person(_, _, _, _, _, _, _))(p => (p.id, p.name, p.born, p.vip, p.score, p.address, p.tags))
  val pet: Shape[Pet] = Shape.sum(Shape.sub[Pet, Dog]("dog", record(field("name", text), field("age", int))(Dog(_, _))(d => (d.name, d.age))) { case d: Dog => Some(d); case _ => None },
    Shape.sub[Pet, Fish.type]("fish", unit.map[Fish.type](_ => Fish)(_ => ())) { case Fish => Some(Fish); case _ => None })

  val derivedPerson: Shape[Person] = Shape.derived[Person]      // Address derived within, as no shape of it is given
  val derivedPet: Shape[Pet] = Shape.derived[Pet]
  val ann = Person("ann", "Ann", Some(Day(1990, 5, 6)), vip = true, Some(7), Some(Address("Kyiv", List("Main 1", "flat 2"))), List("x", "y"))
  val bo = Person("bo", "Bo", None, vip = false, None, None, Nil)

  test("a record derived encodes, decodes and erases as the one written by hand") {
    List(ann, bo).foreach { p =>
      assertEquals(derivedPerson.encode(p), person.encode(p))
      assertEquals(derivedPerson.decode(person.encode(p)), Right(p))
      assertEquals(person.decode(derivedPerson.encode(p)), Right(p))
    }
    assertEquals(derivedPerson.erased, person.erased)
    assertEquals(derivedPerson.decode(Json.obj("id" -> Json.Str("x"))).left.map(_.map(_.render)), Left(List("name: required", "vip: required", "tags: required")))
  }
  test("a sum derived: the cases named from their types, a case object a record of no fields") {
    List[Pet](Dog("Rex", 3), Fish).foreach { p =>
      assertEquals(derivedPet.encode(p), pet.encode(p))
      assertEquals(derivedPet.decode(pet.encode(p)), Right(p))
    }
    assertEquals(derivedPet.erased, pet.erased)
    assertEquals(derivedPet.encode(Fish), Json.obj("type" -> Json.Str("fish")))
    assertEquals(Shape.caseName("KindDefined"), "kind-defined"); assertEquals(Shape.caseName("Fish$"), "fish")
  }
  test("given shapes win over derivation; a nested type derives within; the implicit shapes of the primitives") {
    implicit val city: Shape[Address] = address.described("Where")
    val nested = Shape.derived[Nested]
    val n = Nested(ann, List(Dog("Rex", 3), Fish))
    assertEquals(nested.decode(nested.encode(n)), Right(n))
    assert(Json.print(Schema.shape.encode(nested.erased)).contains("Where"), "the given shape of Address, described, is the one used")
    assertEquals(Shape.of[Option[List[Int]]].encode(Some(List(1, 2))), Json.arr(Json.num(1), Json.num(2)))
    assertEquals(Shape.of[Double].decode(Json.num(2)), Right(2.0))
  }
  test("an implicit shape defined by derivation does not find itself") {
    implicit val self: Shape[Address] = Shape.derived[Address]
    assertEquals(self.decode(self.encode(Address("Kyiv", Nil))), Right(Address("Kyiv", Nil)))
    assertEquals(Shape.of[Option[Address]].encode(None), Json.Null)
  }
  test("a derived shape adjusted by field name: words, a secret, a constraint, a default; a name no field has is refused") {
    val adjusted = derivedPerson.labelled("name", "Name", Some("as printed")).secret("id").defaulted("score", Json.num(1))
      .fieldOf[List[String]]("tags")(f => f.copy(shape = f.shape.check("at least one tag")(_.nonEmpty)))
    val Schema.Record(fs) = adjusted.erased: @unchecked
    assertEquals(fs.find(_.name == "name").map(f => (f.label, f.doc)), Some((Some("Name"), Some("as printed"))))
    assertEquals(fs.find(_.name == "id").map(_.secret), Some(true))
    assertEquals(fs.find(_.name == "score").flatMap(_.default), Some(Json.num(1)))
    assertEquals(adjusted.decode(Json.obj("id" -> Json.Str("x"), "name" -> Json.Str("X"), "vip" -> Json.Bool(false), "tags" -> Json.arr(Json.Str("t")))).map(_.score), Right(Some(1)))
    assertEquals(adjusted.decode(Json.obj("id" -> Json.Str("x"), "name" -> Json.Str("X"), "vip" -> Json.Bool(false), "tags" -> Json.arr())).left.map(_.map(_.render)), Left(List("tags: at least one tag")))
    assertEquals(adjusted.encode(ann), person.encode(ann), "written as before")
    assertEquals(adjusted.decode(person.encode(ann)), Right(ann))
    intercept[IllegalArgumentException](derivedPerson.field("nope")(f => f))
    intercept[IllegalArgumentException](derivedPerson.defaulted("score", Json.Str("x")))
    assertEquals(derivedPerson.described("Who").secret("id").erased match { case Schema.Described(Schema.Record(xs), _, _) => xs.find(_.name == "id").map(_.secret); case _ => None }, Some(true))
  }
  test("at(name): a field checked by the compiler and typed: words, a secret, a typed default, a typed check; a wrong name does not compile") {
    val form = derivedPerson.at("name").labelled("Name").at("name").documented("as printed").at("id").secret.at("score").default(Some(1))
      .at("tags").check("at least one tag")(_.nonEmpty).at("born").shape(Shape.date.optional.described("Born"))
    val Schema.Record(fs) = form.erased: @unchecked
    assertEquals(fs.find(_.name == "name").map(f => (f.label, f.doc)), Some((Some("Name"), Some("as printed"))))
    assertEquals(fs.find(_.name == "id").map(_.secret), Some(true)); assertEquals(fs.find(_.name == "score").flatMap(_.default), Some(Json.num(1)))
    assertEquals(form.decode(Json.obj("id" -> Json.Str("x"), "name" -> Json.Str("X"), "vip" -> Json.Bool(false), "tags" -> Json.arr())).left.map(_.map(_.render)), Left(List("tags: at least one tag")))
    assertEquals(form.decode(person.encode(ann)), Right(ann)); assertEquals(form.encode(ann), person.encode(ann))
    val wrong = compileErrors("""derivedPerson.at("nope").secret""")
    assert(wrong.contains("no field 'nope'"), wrong)
    val mistyped = compileErrors("""derivedPerson.at("score").default("x")""")
    assert(mistyped.nonEmpty, "a default of another type than the field's does not compile")
  }
  test("at(_.field): the field selected by a lambda, its name read at compile time, its type its own") {
    val form = derivedPerson.at(_.name).labelled("Name").at(_.score).default(Some(1)).at(_.tags).check("at least one tag")(_.nonEmpty).at(_.id).secret
    val Schema.Record(fs) = form.erased: @unchecked
    assertEquals(fs.find(_.name == "name").flatMap(_.label), Some("Name")); assertEquals(fs.find(_.name == "id").map(_.secret), Some(true))
    assertEquals(form.decode(Json.obj("id" -> Json.Str("x"), "name" -> Json.Str("X"), "vip" -> Json.Bool(false), "tags" -> Json.arr())).left.map(_.map(_.render)), Left(List("tags: at least one tag")))
    assertEquals(form.decode(person.encode(ann)), Right(ann))
    val notAField = compileErrors("""derivedPerson.at(_.toString).secret""")
    assert(notAField.contains("a field is selected"), notAField)
    assert(compileErrors("""derivedPerson.at(_.score).default(1)""").nonEmpty, "a default of another type than the field's does not compile")
  }
}
