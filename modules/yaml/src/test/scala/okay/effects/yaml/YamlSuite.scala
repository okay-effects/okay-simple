package okay.effects.yaml

import okay.effects.Void
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.parsing.ParseError

/** the effect: one program, its handler outside; a failure with its line and column */
class YamlSuite extends munit.FunSuite {
  val ys = new Yamls[Machine.type](Machine)
  import Machine._
  type R[X] = okay.effects.Sum[Void, YamlParser, X]

  test("a program that reads a configuration, the descent answering") {
    val program: Either[ParseError, Json] ! R = ys.parseYaml[R]("services:\n  web: {image: nginx}\n")
    assertEquals(Machine.run(ys.descent(program)), Right(Json.fromText("""{"services": {"web": {"image": "nginx"}}}""")))
    val docs: Either[ParseError, Vector[Json]] ! R = ys.parseYamlAll[R]("a: 1\n---\nb: 2\n")
    assertEquals(Machine.run(ys.descent(docs)).map(_.size), Right(2))
  }
  test("a failure: line and column") {
    val e = Descent.parse("a: 1\nb: [1, 2\nc: 3").left.toOption.get
    assertEquals((e.expected, e.line), (List("',' or ']'"), 3))
  }
}
