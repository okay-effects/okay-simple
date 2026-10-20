# Backlog

Work found or asked for, not started. Each item: what it is, why, its shape, its tests, what to check first.


## ui: Laminar, Airstream, Waypoint — moved

Moved 2026-10-20 to okay-faces (`specs/web.md` there): the web is an application's face, and okay-faces owns its web
story whole. okay-simple stays the interface and the general modules.

## parsing: errors with a position, a committed choice, the input as a position — DONE 2026-10-20

The three items found writing okay-faces' JSON parser, done the same day in one rewrite of the module and a new
module `json` (commit cd1562f): `Input` an index answered in place with `takeWhile`/`takeIf` as operations, `Expect`
outside the choice so the furthest failure is the error with its line and column, `byNext`/`whileNext` deciding by the
next character with no choice point, `StreamInput` pulling chunks as needed; JSON on it, 100 000 deep on the heap. The
measurements before and after are in docs/modules/json.md ("Measured") and bench.d/. What is still open of them:

- `attempt`/committed `orElse` as parsec has it is NOT done: `orElse` still backtracks always. `byNext` covers data
  formats; a grammar needing "try p, else q, but commit once p consumed" has no word yet.
- A stream parse rolled back over a chunk boundary pulls the chunk again; the source must remember. A buffering
  `StreamInput` that keeps chunks since the earliest live choice point would lift that rule.

## shape: derived from a type, Scala 3 first

`Shaped[P]` a type class with `shape: Shape[P]`; `derives Shaped` in `scala-3/` by `Mirror` and `inline`, no
dependency; Scala 2 by hand until a blackbox macro is worth writing (trigger: the first time writing a record by hand
hurts). Derivation gives the skeleton — names and types; labels, docs, secrets, constraints and defaults are added on
the data by field name, `.secret("number")`, `.labelled("issued", "Issued")`, checked by a test that the names exist.
