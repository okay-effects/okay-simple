# json — JSON parsed by effects

`okay-effects-json`, `okay.effects.json` — `import okay.effects.json.*`, the module on the machine over the default
streams. `Json` is the value — `Null`, `Bool`, `Num(BigDecimal)`, `Str`, `Arr(Vector)`, `Obj(Vector[(String, Json)])`,
an object ORDERED as written — `Json.print` and `Json.pretty` its text; and the parser is a program of the
[parsing](parsing.md) module's three effects, the grammar decided by the next character: no choice point, no
backtracking, one operation per token, on the heap.

| | |
|---|---|
| the value | `Json`: `Null`, `Bool(b)`, `Num(n)`, `Str(s)`, `Arr(items)`, `Obj(fields)`; `Json.obj(...)`, `arr(...)`, `num(...)`, `str(...)` |
| text of a value | `Json.print(j)`, `Json.pretty(j, indent = 2)` |
| a text parsed | `parse(text, maxDepth = 1000)`: `Either[ParseError, Json]` (on the machine); `parser(text)` the program; `value` the grammar, for a parser of one's own |
| a stream parsed | `values(chunks, maxDepth)`: a `Stream[Either[ParseError, Json], R]` of the values as they come, whitespace between them — JSON lines, or values concatenated; a failure is one `Left`, the last element |

```scala
import okay.effects.*
import okay.effects.json.*
import okay.effects.json.Json.*

val text = """{"name": "Ann", "tags": ["a", "b"], "age": 41, "note": null}"""
assert(parse(text) == Right(obj("name" -> Str("Ann"), "tags" -> arr(Str("a"), Str("b")), "age" -> num(41), "note" -> Null)))
assert(parse(Json.print(obj("x" -> Num(BigDecimal("1.50"))))) == Right(obj("x" -> Num(BigDecimal("1.50")))))
assert(parse("[1, 2").left.map(_.message) == Left("at line 1, column 6: expected ',' or ']', found end of input"))
assert(parse("""{"a": }""").left.map(e => (e.offset, e.expected)) == Left((6L, List("a JSON value"))))
assert(parse("[" * 100000 + "]" * 100000, maxDepth = 100001).isRight)                // the heap's, not the stack's
assert(parse("[" * 10 + "]" * 10, maxDepth = 5).left.map(_.expected) == Left(List("nesting of at most 5")))
```

Strict as RFC 8259: no leading zero, no control character in a string, the escapes `\" \\ \/ \b \f \n \r \t \uXXXX`,
whitespace a space, a tab, a line feed, a carriage return. Numbers are `BigDecimal`, as written.

## Values as they come

```scala
import okay.effects.stream.*

val lines = fromIterator[String, Pure](Iterator("{\"a\": 1}\n{\"b\":", " 2}\n", "3 \"x\""))
assert(run(values(lines).toList) == List(Right(obj("a" -> num(1))), Right(obj("b" -> num(2))), Right(num(3)), Right(Str("x"))))
assert(run(values(fromIterator[String, Pure](Iterator("1 ", "[2"))).toList).map(_.left.map(_.offset)) == List(Right(num(1)), Left(4L)))
```

Each value is parsed when the stream is pulled, the chunks pulled as the parser needs them: a file of JSON lines is
read as far as it is asked. The `Left` at a failure is the last element, the stream cannot resynchronise after it.

## Measured

The point of this module is that a parser on effects can be fit for real life. The same grammar on the first
version of the parsing module — the input `State[String]`, each character a `substring(1)` — was QUADRATIC: 1.5 s and
over 1 GB of heap on 100 KB, 3200× a hand-written loop. With the input as an index answered in place and the grammar
decided by the next character, the module's cost is the handlers' alone. Measured where it lands is in
`bench/` (`JsonBenchmark`), and the numbers are kept in BACKLOG.md's "parsing" items as they are taken.
