# json — JSON: the value, the effect, two handlers

`okay-effects-json`, `okay.effects.json` — `import okay.effects.json.*`, the module on the machine over the default
streams. `Json` is the value — `Null`, `Bool`, `Num(BigDecimal)`, `Str`, `Arr(Vector)`, `Obj(Vector[(String, Json)])`,
an object ORDERED as written — `Json.print` and `Json.pretty` its text. PARSING IS AN EFFECT, `JsonParser`: a program
says `parseJson(text)` and no more; HOW is the handler's choice:

- `descent`: a hand-written recursive descent, an index over the text, answered in place — the fast one, and the
  default of `parse(text)`;
- `grammar`: the grammar as a program of the [parsing](parsing.md) module's three effects, decided by the next
  character, no choice point, on the heap — the one `values` streams with, and the proof that a parser on effects is
  fit for real life.

The two give the same values and the same errors, which the suite holds them to; a program written on the effect runs
under either, and a third handler — another encoding, a cache, a test's table — is one more.

| | |
|---|---|
| the value | `Json`: `Null`, `Bool(b)`, `Num(n)`, `Str(s)`, `Arr(items)`, `Obj(fields)`; `Json.obj(...)`, `arr(...)`, `num(...)`, `str(...)` |
| text of a value | `Json.print(j, ascii = false)`, `Json.pretty(j, indent = 2, ascii = false)` — iterative, any depth |
| the way out, an effect | `writeJson(j)`: `Unit ! R`, `JsonWriter` in `R`; WHERE is the handler's: `collected(format, separator)` into a string, `writing(sink, format)` to any program taking the text — a console, a socket — `streamed(format)` as a stream; HOW is the `Format`: `Compact(ascii)`, `Indented(indent, ascii)` |
| a stream written | `chunks(j)`: a `Stream[String, R]` of the value's pieces as it is walked, the dual of `values`; `lines(values)`: JSON lines, a value a line |
| the effect | `parseJson(text, maxDepth = 1000)`: `Either[ParseError, Json] ! R`, `JsonParser` in `R`; `descent(p)` and `grammar(p)` its handlers |
| at once | `parse(text, maxDepth)`: `Either[ParseError, Json]`, by descent; `parser(text)` the grammar as a program; `value` the grammar, for a parser of one's own |
| a stream parsed | `values(chunks, maxDepth)`: a `Stream[Either[ParseError, Json], R]` of the values as they come, whitespace between them — JSON lines, or values concatenated; a failure is one `Left`, the last element |

```scala
import okay.effects.*
import okay.effects.json.*
import okay.effects.json.Json.*
import okay.effects.parsing.ParseError

val text = """{"name": "Ann", "tags": ["a", "b"], "age": 41, "note": null}"""
assert(parse(text) == Right(obj("name" -> Str("Ann"), "tags" -> arr(Str("a"), Str("b")), "age" -> num(41), "note" -> Null)))
assert(parse(Json.print(obj("x" -> Num(BigDecimal("1.50"))))) == Right(obj("x" -> Num(BigDecimal("1.50")))))
assert(parse("[1, 2").left.map(_.message) == Left("at line 1, column 6: expected ',' or ']', found end of input"))
assert(parse("""{"a": }""").left.map(e => (e.offset, e.expected)) == Left((6L, List("a JSON value"))))
assert(parse("[" * 10 + "]" * 10, maxDepth = 5).left.map(_.expected) == Left(List("nesting of at most 5")))

// the effect: one program, the handler chosen outside it
type R = JsonParser
val program: Either[ParseError, Json] ! R = parseJson[R]("""[1, {"two": 2}]""")
assert(run(descent(program)) == run(grammar(program)))
assert(run(grammar(parseJson[R]("[" * 100000 + "]" * 100000, maxDepth = 100001))).isRight)   // the grammar: the heap's, not the stack's
```

Strict as RFC 8259: no leading zero, no control character in a string, the escapes `\" \\ \/ \b \f \n \r \t \uXXXX`,
whitespace a space, a tab, a line feed, a carriage return. Numbers are `BigDecimal`, as written.

## The way out

Writing is the effect `JsonWriter`, as parsing is `JsonParser`: a program says `writeJson(j)` and no more. WHERE the
text goes is the handler's — `collected` into one string (JSON lines by default), `writing(sink)` to any program of the
row that takes a string, a console's line, a socket's send, a file's append, `streamed` as a stream pulled as the
program writes — and HOW is the `Format` the handler is given, compact for a wire, indented for a person, ASCII-only
for a channel that wants it. The printer underneath is ITERATIVE, an explicit stack of what is still to write, so the
value nested a hundred thousand deep that the grammar parsed on the heap prints on the heap too. `chunks(j)` writes
one value as a stream of its pieces; `lines(values)` writes JSON lines.

```scala
import okay.effects.std.*
import okay.effects.stream.*

type W = JsonWriter
val out: Unit ! W = writeJson[W](obj("a" -> arr(num(1), Str("é")))).flatMap(_ => writeJson[W](num(7)))
assert(run(collected()(out)) == ("""{"a":[1,"é"]}""" + "\n7", ()))
assert(run(collected(Format.Compact(ascii = true))(out))._1 == """{"a":[1,"\u00e9"]}""" + "\n7")
assert(run(collected(Format.Indented())(out))._1 == "{\n  \"a\": [\n    1,\n    \"é\"\n  ]\n}\n7")
assert(run(streamed[Pure]()(out).toList) == List("""{"a":[1,"é"]}""", "7"))
type WT = Writer % String + JsonWriter
assert(run(writer[String](writing[Writer % String](s => tell[String, Writer % String](s))(out.into[WT])))._1 == List("""{"a":[1,"é"]}""", "7"))
assert(run(chunks[Pure](obj("a" -> arr(num(1)))).toList).mkString == """{"a":[1]}""")
assert(run(lines[Pure](emits[Json, Pure](List(num(1), Null))).toList) == List("1\n", "null\n"))
```

## Values as they come

```scala
val input = fromIterator[String, Pure](Iterator("{\"a\": 1}\n{\"b\":", " 2}\n", "3 \"x\""))
assert(run(values(input).toList) == List(Right(obj("a" -> num(1))), Right(obj("b" -> num(2))), Right(num(3)), Right(Str("x"))))
assert(run(values(fromIterator[String, Pure](Iterator("1 ", "[2"))).toList).map(_.left.map(_.offset)) == List(Right(num(1)), Left(4L)))
```

Each value is parsed when the stream is pulled, the chunks pulled as the parser needs them: a file of JSON lines is
read as far as it is asked. The `Left` at a failure is the last element, the stream cannot resynchronise after it.

## Measured

The point of the grammar is that a parser on effects can be fit for real life; the point of the effect is that the
choice is the handler's, by a number, not a principle. The same grammar on the first version
of the parsing module — the input `State[String]`, each character a `substring(1)`, the grammar on `orElse` and `many`
— was QUADRATIC: 1.5 s and over 1 GB of heap on 100 KB, 3200× a hand-written loop. With the input an index answered in
place, `takeWhile` an operation, and the grammar decided by the next character, the cost is the handlers' alone and
linear (JMH, `bench/JsonBenchmark`, bench.d/2026-10-20_cd1562f_json-effects-vs-hand.txt):

| text | `descent` | `grammar` on the machine | `grammar` on `Free` |
|---|---|---|---|
| 5 KB | 10 µs | 195 µs, 19× | 243 µs, 23× |
| 100 KB | 204 µs | 3.9 ms, 19× | 4.8 ms, 23× |

Twenty times the text, twenty times the time. Nineteen times a hand loop is the price of every token being an
operation found by its handler and answered through the machine; where it is spent, and what of it the machine can
shed, is the next measurement, not a guess.
