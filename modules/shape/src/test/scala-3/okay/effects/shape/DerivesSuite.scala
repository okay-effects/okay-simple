package okay.effects.shape

import okay.effects.json.Json

/** Scala 3's `derives`: a type says it has a shape, the companion holds it, `summon` and `Shape.of` find it — and a
 * type derived that way is a field of another, found as given */
final case class Coord(lat: Double, lon: Double) derives Shape
final case class Place(name: String, at: Coord, aliases: List[String]) derives Shape
enum Colour derives Shape:
  case Red, Green
  case Custom(hex: String)
final case class Paint(colour: Colour, coats: Option[Int]) derives Shape

class DerivesSuite extends munit.FunSuite:
  test("derives Shape: the companion's given, summoned, used within another type"):
    val place = summon[Shape[Place]]
    val p = Place("home", Coord(50.45, 30.52), List("Kyiv"))
    assertEquals(place.decode(place.encode(p)), Right(p))
    assertEquals(place.encode(p),
      Json.obj("name" -> Json.Str("home"),
        "at" -> Json.obj("lat" -> Json.Num(BigDecimal("50.45")), "lon" -> Json.Num(BigDecimal("30.52"))),
        "aliases" -> Json.arr(Json.Str("Kyiv"))))
    assert(Shape.of[Coord] eq Coord.derived$Shape, "the companion's one")
  test("an enum derives: simple cases records of no fields, a case with fields a record, named by caseName"):
    val paint = Shape.of[Paint]
    List(Paint(Colour.Red, None), Paint(Colour.Custom("#ff0"), Some(2))).foreach(x =>
      assertEquals(paint.decode(paint.encode(x)), Right(x)))
    assertEquals(Shape.of[Colour].encode(Colour.Green), Json.obj("type" -> Json.Str("green")))
    assertEquals(Shape.of[Colour].encode(Colour.Custom("#ff0")),
      Json.obj("type" -> Json.Str("custom"), "hex" -> Json.Str("#ff0")))
