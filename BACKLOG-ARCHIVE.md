# Backlog, archived

Items done or dropped, as they were written, for the record.

## parsing: errors with a position and a message

Found 2026-10-20 writing okay-face's `Json.parse`: the module could not be used for it. `parse(input)(p)` answers
`Option[A]`: a failed parse says nothing of where or why, and a person with a damaged file needs "at offset 17:
expected ',' or '}'". The JSON grammar in docs/modules/parsing.md shows the module parsing; this is what it lacks
to be USED.

**Shape.** The input as a POSITION into a fixed text rather than the text left (`State[Int]` with the text in a
`Reader`, so `item` is an index and a substring is never copied); the furthest position reached and what was
expected there, kept outside the choice (the standard furthest-failure rule, as parsec merges errors: a `State` of
`(offset, Set[String])` the handler of `Choose` does not roll back); `label(p, "a digit")` naming what a parser
expects; `parseEither(input)(p): Either[ParseError, A]` with offset, line and column, and the expectations; `parse`
kept as it is, `parseEither(...).toOption`.

**Tests.** The arithmetic example: `2+` fails at offset 2 expecting a digit or `(`; `(1` at 2 expecting `)`, `*` or
`+`. JSON: `[1, 2` fails at the end expecting `,` or `]`; `{"a": }` at the `}` expecting a value. Both
implementations, both compilers. okay-face's agreement test then compares messages too, not only success.

**Check first.** Whether the furthest-failure state can sit outside `first` and still be updated by every branch (the
`Choose` handler's `k` resumed per alternative, the state handled outside it is shared); how `Input` as an index
changes the module's words (`item`, `satisfy`, `string`, `eof` only).

## parsing: a committed choice, no backtracking, for grammars that decide by the next character

Found the same day, the same way. `orElse` backtracks by `Choose`: if the whole parse fails one way, the other
alternative is tried from the same input. JSON, like most data formats, decides by the first character; backtracking
there is wasted work, on a failure deep in nested `many`s it is exponential, and it loses the real error, because
the furthest failure is abandoned for the alternative.

**Shape.** As parsec: `orElse` tries `q` only when `p` failed WITHOUT consuming input; `attempt(p)` makes `p` fail
without consuming, so backtracking is asked for where it is wanted (`string("let")` against `string("letter")`).
Needs the position above to know "consumed". Or, simpler and enough for data formats, `oneOf(next: Char => Option[A ! R])`:
the alternative chosen by the next character, no `Choose` at all for it.

**Tests.** The JSON grammar on the committed choice parses the same values as on `orElse`; a failure deep in a nested
array reports its position; `string("let")` / `string("letter")` still parses `letter` under `attempt`. **Measure**, in
bench: the JSON grammar on the module against a hand-written parser (okay-face's `Json.parse`, the same `Value`) on a
100 KB text, the ratio recorded; the module's parsers should be within a small factor, not orders.

**Check first.** How a failure past the choice is expressed: an `Abort` or `Error` raised and handled OUTSIDE `first`,
so the choice sees no alternative to try; whether `first` can be told that from inside.

## parsing: the input as a position, not the text left — measured

Measured 2026-10-20 (okay-face, a scratch suite since removed): the JSON grammar of docs/modules/parsing.md on the
machine against a hand-written recursive descent, the same `Value`, JVM, warm:

| text | hand | module | ratio |
|---|---|---|---|
| 4.8 KB (the meta-schema) | 0.07 ms | 5.9 ms | 80× |
| 10 KB | 0.07 ms | 14 ms | 190× |
| 100 KB | 0.46 ms | 1465 ms, and over 1 GB of heap | 3200× |

Ten times the text, a hundred times the time: QUADRATIC. The cause is `Input = State[String]` with `item` doing
`in.substring(1)` — a copy of the rest of the text per character, O(n) each, so O(n²) in all, and every choice point
holds its own copy. This is the module's representation, not the machine's or the effects' cost. With the input as
an index into a fixed text (`State[Int]`, the text in a `Reader` or closed over), the per-character cost is the
handlers' alone: a `get`, a `put`, a `guard`, and for `many` a choice point with a captured `k` — expected within one
or two orders of a loop, to be measured again then, not guessed.

Nesting: `[` × n `]` × n, default stack and heap — both parse 20 000 deep; at 100 000 the hand parser dies of
`StackOverflowError` (recursion on the JVM stack), the module of `OutOfMemoryError` (the quadratic copies). The
machine is heap-based, so a parser on it is stack-safe by construction; with the index representation it should take
100 000 deep. That is a real argument FOR the module on untrusted input (an MCP client's message), where a
hand-written parser needs a depth limit to turn a stack overflow into a problem.

**Shape.** `Input` as a position: `item` reads `text.charAt(i)` and `put(i + 1)`; `parse(text)(p)` closes over the
text; the module's words change in their bodies only. Then the two items above (errors at the furthest position; a
committed choice) build on the position. Then the measurement again, in bench, recorded.

## shape: reading as an effect — DONE 2026-10-20 (`Fields`, `Reading`, `ShapeAlgebra`)

Decided 2026-10-20 (okay-face specs/face-core.md, "Reading a shape is an effect"), the module moved here the same day.
`decode(v: Json)` fixes the fields' source to a JSON value; nothing in a `Shape` says JSON. Reading the fields a shape
names is a program over an effect — `Fields`: `Read(path, prim)`, `Enter(path)`, `Leave`, `Size(path)` — and the
source is the handler's: a `Json` (today's decode), another encoding (CBOR), a command line's arguments, a PERSON asked
field by field at a terminal, with the problem shown and the field asked again — okay-face's terminal face is that
handler, not code of its own. `read(shape): Either[Problems, P] ! Fields` keeps every problem as `decode` does;
`decode(v)` stays as `run(fromJson(v)(read(shape)))`. Encoding stays a function to a value. Tests: `fromJson` agrees
with `decode` on every ShapeSuite case; a scripted "person" handler answers prompts in order and is asked again on a
problem; the two handlers give the same `P` for the same answers.

