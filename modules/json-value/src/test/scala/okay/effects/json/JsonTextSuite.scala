package okay.effects.json

/** the value module alone: text to a value and back, the order of fields kept, a failure saying where */
class JsonTextSuite extends munit.FunSuite {
  test("parsed and printed back, fields in order") {
    val text = """{"b":1,"a":[true,null,"x\né"],"n":-1.25,"o":{}}"""
    val v = JsonText.parse(text)
    assertEquals(v.map(Json.print(_)), Right(text.replace("\\u00e9", "é")))
    assertEquals(v.toOption.collect { case o: Json.Obj => o.fields.map(_._1) }, Some(Vector("b", "a", "n", "o")))
  }
  test("a failure: where, and what was expected") {
    assertEquals(JsonText.parse("""{"a": 01}"""), Left(JsonText.Error(7, "no leading zero")))
    assertEquals(JsonText.parse("[1, 2"), Left(JsonText.Error(5, "',' or ']'")))
    assertEquals(JsonText.parse("[[1]]", maxDepth = 1).left.map(_.expected), Left("nesting of at most 1"))
  }
  test("read: fields, values at their types, a wrong one said") {
    val v = Json.fromText("""{"name": "x", "n": 3, "xs": [1, 2.5], "on": true, "none": null}""")
    assertEquals((v("name").string, v("n").int, v("xs").list.map(_.double), v("on").boolean),
      ("x", 3, Vector(1.0, 2.5), true))
    assertEquals((v("missing"), v.field("none"), v.field("n")), (Json.Null, None, Some(Json.num(3))))
    assertEquals(v.record.map(_._1), Vector("name", "n", "xs", "on", "none"))
    assertEquals(intercept[IllegalArgumentException](v("name").int).getMessage, "a number expected, text found")
    assertEquals(Json.quote("a\"b\n"), "\"a\\\"b\\n\"")
    assertEquals(Json.fromText("[1,{}]").render, "[1,{}]")
  }
}
