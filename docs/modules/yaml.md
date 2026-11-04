# yaml — YAML: the value, the effect, its handler

`okay-effects-yaml`, `okay.effects.yaml` — `import okay.effects.yaml.*`, the module on the machine. THE VALUE ALONE is a
module of its own, `okay-effects-yaml-value` — `YamlText.parse` (one document), `YamlText.parseAll` (documents
between `---`), `YamlText.print` (a value as block YAML, read back to the same value) — depending on nothing but the
JSON value: YAML's values are [json](json.md)'s `Json`, objects ordered as written. The parser is a hand-written
recursive descent over the text's lines and their indentation, as `JsonText` is over its characters; a failure is an
offset and what was expected there. PARSING IS AN EFFECT, `YamlParser`: a program says `parseYaml(text)` or
`parseYamlAll(text)` and no more; `descent` answers it in place, its failure the [parsing](parsing.md) module's
`ParseError` (line and column).

What is read — what configurations are written in (docker compose, Kubernetes, CI files):

| | |
|---|---|
| blocks | mappings and sequences by indentation; a sequence under a key at the key's own indentation; `- key: v` items |
| flow | `[a, b]`, `{a: 1, b: [2]}`, across lines, `[k: v]` a one-field mapping |
| scalars | plain (continued on more indented lines), `'single'` (`''`), `"double"` (`\n \t \x.. \u.... \U........`, line folding) |
| blocks of text | literal `\|` and folded `>`, chomping `-` `+`, an indentation digit |
| the rest | comments, anchors `&a` and aliases `*a`, the merge key `<<` (written keys win), tags (`!!str` kept, others read past), documents `---` / `...`, directives `%` |
| resolution | YAML 1.2's core schema: `null` `~` and empty, `true`/`false`, integers (`0o`, `0x` too), floats; `.inf` and `.nan` kept as text (JSON has no such numbers) |
| not read | complex keys (`? `), a key spanning lines |

| | |
|---|---|
| the value | `YamlText.parse(text, maxDepth = 1000)`: `Either[YamlText.Error, Json]`; `parseAll` for documents; `print(j)` |
| the effect | `parseYaml(text)`, `parseYamlAll(text)`: `Either[ParseError, …] ! R`, `YamlParser` in `R`; `descent(p)` its handler |
| at once | `parse(text)`: `Either[ParseError, Json]`, by descent; `print(j)` |

The suite holds the printer and the parser to each other over random values (`parse(print(v)) == v`), and reads a
compose file, Kubernetes manifests, anchors and merges, block scalars and the core schema's scalars.
