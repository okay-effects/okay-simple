# shape — a typed schema: a form, a tool's input schema, the decoder

`okay-effects-shape`, `okay.effects.shape` — `import okay.effects.shape.*`. A `Shape[P]` says what a value `P` looks
like, and is at once a FORM for a person (fields, labels, constraints), an `inputSchema` for an agent's tool, a request
body, a command's flags — and the DECODER that validates what any of them sends, every problem with its path, not the
first. Written once, derived from everywhere. `erased` is the same shape as a value, `Schema`, for files and wires;
`Schema.loose` reads a value against an erased shape; `Schema.shape` is the meta-schema, a `Schema` read and written
by the same decoder as everything else. `JsonSchema.of` is the JSON Schema (draft 2020-12) of a `Schema`.

| | |
|---|---|
| primitives | `text`, `integer`, `int`, `long`, `number`, `bool`, `date` (`Day`), `any`, `choice(name -> a, ...)`, each with `Constraint`s as data |
| compound | `record(fields...)(make)(unmake)` for 1 to 12 fields; `sum(cases...)` with `sub` and `Case`; `.optional`, `.many(min, max)`; `deferred(name)(shape)` for recursion |
| refined | `.map(to)(from)`, `.refine(to)(from)`, `.check(message)(ok)`, `.described(label, doc)` |
| a field | `field(name, shape)`, `.labelled`, `.documented`, `.asSecret`, `.withDefault` |
| decoding | `shape.decode(json)`: `Either[Problems, P]`, a `Problem(path, message)` per fault; `shape.encode(p)`; a missing field is its default, else `None` for an `Optional`, else `required`; unknown fields are ignored |
| erased | `shape.erased: Schema`; `Schema.loose(schema, defs): Shape[Json]`; `Schema.shape: Shape[Schema]`, `Schema.defs` |
| JSON Schema | `JsonSchema.of(schema, defs)`: `title`, `description`, `default`, `enum`, `oneOf` with a `type` tag for a sum, `$ref` into `$defs`, `"x-secret": true` |

```scala
import okay.effects.json.Json
import okay.effects.json.Json.*
import okay.effects.shape.*
import okay.effects.shape.Shape.*

final case class Person(name: String, age: Int, email: Option[String], born: Day)
val person: Shape[Person] = record(
  field("name", text(Constraint.MinLength(1))).labelled("Name"),
  field("age", int).documented("in years"),
  field("email", text(Constraint.Pattern(".+@.+", "an email address")).optional),
  field("born", date))(Person(_, _, _, _))(p => (p.name, p.age, p.email, p.born))

val ann = Person("Ann", 41, Some("ann@x.io"), Day(1985, 3, 9))
assert(person.encode(ann) == obj("name" -> Str("Ann"), "age" -> num(41), "email" -> Str("ann@x.io"), "born" -> Str("1985-03-09")))
assert(person.decode(person.encode(ann)) == Right(ann))
assert(person.decode(obj("name" -> Str(""), "age" -> Str("old"), "born" -> Str("1985-13-01"))) ==
  Left(List(Problem(List("name"), "at least 1 characters"), Problem(List("age"), "expected a whole number, got text"), Problem(List("born"), "not a date: 1985-13-1"))))
assert(Json.print(JsonSchema.of(person.erased)).startsWith("""{"type":"object","properties":{"name":{"type":"string","minLength":1,"title":"Name"}"""))
assert(Schema.shape.decode(Schema.shape.encode(person.erased)) == Right(person.erased))   // a schema is a value too
```

A `Sum` is a tagged union, `{"type": name, ...the case's fields}` for a record case and `{"type": name, "value": v}`
otherwise. `Day(y, m, d)` is a calendar date of its own, ISO on the wire, so that Scala.js and Native need no java.time.

## An algebra of types

A `Shape` is the universe of sums of products: `Record` a product, `Sum` a coproduct, `Optional` and `Many` the type
constructors over a shape, `Mapped` an isomorphism or a refinement, `Deferred` with `Ref` the fixed point — recursion.
That is the universe a compiler's `Mirror` describes too, so derivation from a type (BACKLOG.md) is a translation
between the two. Every interpreter — the erasure, the JSON Schema, the validation, a form — is a fold over this
tree; `Fields` is `algebra`'s free applicative, so a record's fields run in any Applicative and are listed without
running.

## Why a shape is data

Whatever produces a shape — written by hand, derived from a type by the compiler, read from a file as a `Schema`
(a user-defined kind of record) — the rest is interpreters of the same data: the decoder, the form, the JSON Schema,
a terminal's prompts. Reading the fields a shape names is itself an effect, the source of the fields a handler's — a
JSON value, another encoding, a command line, a person asked field by field; that is the next step of this module
(BACKLOG.md, "shape: reading as an effect").
