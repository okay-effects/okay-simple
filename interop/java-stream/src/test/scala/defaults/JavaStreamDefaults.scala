package defaults

import java.util.stream.{Stream => JStream}
import okay.effects._
import okay.effects.stream._
import okay.effects.jstream._

class JavaStreamDefaults extends munit.FunSuite {
  test("import okay.effects.jstream._") {
    assertEquals(run(fromJava[Int, Pure](JStream.of(1, 2, 3)).map(_ * 2).toList), List(2, 4, 6))
  }
}
