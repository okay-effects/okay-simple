package okay.effects.shape

import okay.effects.json.Json

import Shape._

class JsonSchemaSuite extends munit.FunSuite {
  final case class Order(item: String, count: Int, note: Option[String], rush: Boolean, pin: String)
  val order: Shape[Order] = record(
    field("item", text(Constraint.MinLength(1), Constraint.MaxLength(40))).labelled("Item"),
    field("count", int.check("positive")(_ > 0)).documented("how many"),
    field("note", text.optional),
    field("rush", bool).withDefault(false),
    field("pin", text).asSecret)(Order(_, _, _, _, _))(o => (o.item, o.count, o.note, o.rush, o.pin))

  test("a record's JSON Schema") {
    assertEquals(Json.print(JsonSchema.of(order.erased)),
      """{"type":"object","properties":{""" +
        """"item":{"type":"string","minLength":1,"maxLength":40,"title":"Item"},""" +
        """"count":{"type":"integer","description":"how many"},""" +
        """"note":{"type":"string"},""" +
        """"rush":{"type":"boolean","default":false},""" +
        """"pin":{"type":"string","x-secret":true}},""" +
        """"required":["item","count","pin"]}""")
  }
  test("the primitives") {
    assertEquals(Json.print(JsonSchema.node(Schema.Date)), """{"type":"string","format":"date"}""")
    assertEquals(Json.print(JsonSchema.node(Schema.Choice(List("a", "b")))), """{"type":"string","enum":["a","b"]}""")
    assertEquals(Json.print(JsonSchema.node(Schema.Many(Schema.Integer(List(Constraint.Min(0))), 1, Some(5)))),
      """{"type":"array","items":{"type":"integer","minimum":0},"minItems":1,"maxItems":5}""")
    assertEquals(Json.print(JsonSchema.node(Schema.Optional(Schema.Bool))),
      """{"anyOf":[{"type":"boolean"},{"type":"null"}]}""")
    assertEquals(Json.print(JsonSchema.node(Schema.Described(Schema.Number(Nil), "Price", Some("in euros")))),
      """{"type":"number","title":"Price","description":"in euros"}""")
    assertEquals(Json.print(JsonSchema.node(Schema.Any)), "{}")
  }
  test("a sum is oneOf with a type tag") {
    val s = Schema.OneOf(Vector("dog" -> Schema.Record(Vector(Schema.Field("name", Schema.Text(Nil), None, None, false,
              None))), "n" -> Schema.Integer(Nil)))
    assertEquals(Json.print(JsonSchema.node(s)),
      """{"oneOf":[{"type":"object","properties":{"type":{"type":"string","enum":["dog"]},""" +
        """"name":{"type":"string"}},"required":["type","name"]},""" +
        """{"type":"object","properties":{"type":{"const":"n"},"value":{"type":"integer"}},""" +
        """"required":["type","value"]}]}""")
  }
  test("refs and defs: the meta-schema's own JSON Schema is finite and refers to itself") {
    val v = JsonSchema.of(Schema.shape.erased, Schema.defs)
    val text = Json.print(v)
    assert(text.contains("\"$ref\":\"#/$defs/schema\""))
    assert(text.contains("\"$defs\":{\"schema\":{\"oneOf\":["))
    assert(text.length < 20000)
  }
}
