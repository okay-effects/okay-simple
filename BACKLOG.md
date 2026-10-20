# Backlog

Work found or asked for, not started. Each item: what it is, why, its shape, its tests, what to check first.


## ui: Laminar, Airstream, Waypoint — moved

Moved 2026-10-20 to okay-face (`specs/web.md` there): the web is an application's face, and okay-face owns its web
story whole. okay-simple stays the interface and the general modules.

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
