# Backlog

Work found or asked for, not started. Each item: what it is, why, its shape, its tests, what to check first.


## ui: Laminar, Airstream, Waypoint — moved

Moved 2026-10-20 to okay-faces (`specs/web.md` there): the web is an application's face, and okay-faces owns its web
story whole. okay-simple stays the interface and the general modules.

## parsing: errors with a position, a committed choice, the input as a position — DONE 2026-10-20, the rest 2026-10-22

The three items found writing okay-faces' JSON parser, done the same day in one rewrite of the module and a new
module `json` (commit cd1562f): `Input` an index answered in place with `takeWhile`/`takeIf` as operations, `Expect`
outside the choice so the furthest failure is the error with its line and column, `byNext`/`whileNext` deciding by the
next character with no choice point, `StreamInput` pulling chunks as needed; JSON on it, 100 000 deep on the heap. The
measurements before and after are in docs/modules/json.md ("Measured") and bench.d/. What is still open of them:

- DONE 2026-10-22: the committed choice, `orCommitted(p, q)` and `manyCommitted(p)` — parsec's `p <|> q`, `orElse`
  staying `try p <|> q`. The attempts are kept by the expectations' handler, outside the choice: opened at their start,
  closed by success, asked by the other alternative; a stack, so each step is O(1).
- DONE 2026-10-22: the stream input remembers what it pulled (`StreamInput.remembered`): a rolled-back alternative
  reads the chunks already pulled, so the source need not remember; what is kept is what a live choice point reaches.

## parsing: a committed choice that frees its alternatives (kind: perf) — found 2026-10-22, DONE 2026-10-22

`orCommitted` and `manyCommitted` keep the other branch on the choice's stack until the parse ends; the commitment
is checked when that branch runs, and it then fails at once. parsec drops the branch and runs `many` in constant
space. Measured (bench.d/2026-10-22_f16fee9_parsing-scale.txt): allocation per term flat, time per term 266 -> 734 ns
from 2000 to 32000 terms on the machine, against 177 -> 320 ns for the backtracking `many` — the live heap grows with
the input. The shape of a fix: a `cut` in `Choose` (drop the alternatives made since a mark), issued when the attempt
commits; or a committed choice handled by its own handler. Expected: flat time per term for `manyCommitted`.
Disqualifying: if the per-term rise stays after the alternatives are dropped, the retention is elsewhere (the `map`
chain of `many`'s results).
DONE: `Choose` got `Mark`/`Cut` and `once`; `first` and `all` keep the pending alternatives as an explicit stack and
resume in tail position; `orCommitted` cuts once `p` succeeds; `manyCommitted` is a loop of item-wise committed
choices. Time per term on the machine 304 -> 381 ns from 2000 to 32000 terms (was 266 -> 734); bench.d/2026-10-22_cut.txt.

## the machine's `first` as a stateful handler: 15% slower in backtracking search (kind: perf) — found 2026-10-22

`first` on an explicit stack (for the cut) threads its state through every resume: on the machine, `firstSearch`
(65 536 ways) 4307 -> 4958 us, `firstChain` 1524 -> 1626 us; on `Free` 21% faster, and `all` 2.2-3.5x faster on both
(bench.d/2026-10-22_cut.txt). Where to look: the machine's path for a StatefulHandler against a Handler — the state
boxed per resume, the clause called through the pair. Expected: `firstSearch` back to ~4300 us. Disqualifying: if a
Handler-shaped `first` keeping its stack in the answer type (`B = Pending => Option[A] ! Rest`) is no faster, the
cost is the stack's allocation, not the stateful path.

## shape: derived from a type, Scala 3 first

`Shaped[P]` a type class with `shape: Shape[P]`; `derives Shaped` in `scala-3/` by `Mirror` and `inline`, no
dependency; Scala 2 by hand until a blackbox macro is worth writing (trigger: the first time writing a record by hand
hurts). Derivation gives the skeleton — names and types; labels, docs, secrets, constraints and defaults are added on
the data by field name, `.secret("number")`, `.labelled("issued", "Issued")`, checked by a test that the names exist.
