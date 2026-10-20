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
}
