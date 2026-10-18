# Backlog

Work found or asked for, not started. Each item: what it is, why, its shape, its tests, what to check first.

## ui: Laminar, Airstream, Waypoint

Asked 2026-10-18 (moved here from okay's backlog: the effects and streams side fits okay-simple, which is released;
okay-ui's own host and its typed DOM vocabulary stay in okay, `backlog.d/okay-ui/okay-laminar.md`).

Three modules in a new directory `ui/`, Scala.js only, both compilers, each a class over any `Runner` with its default
on the machine, as the other modules are:

| Module | Artifact | Package | Depends on |
|---|---|---|---|
| `ui/airstream` | `okay-effects-airstream` | `okay.effects.airstream` | stream, async; `com.raquo::airstream` |
| `ui/laminar` | `okay-effects-laminar` | `okay.effects.laminar` | ui/airstream; `com.raquo::laminar` |
| `ui/waypoint` | `okay-effects-waypoint` | `okay.effects.waypoint` | ui/laminar; `com.raquo::waypoint` |

Versions (Maven Central, 2026-10-18): Laminar and Airstream 17.2.1, published for Scala 3 (on 3.3.1) and 2.13;
Waypoint 9.0.0, on Laminar 17.2.0 and url-dsl 0.6.2. 18.0.0-M5 / 10.0.0-M7 are milestones: take the stable ones.

**ui/airstream — Airstream's streams and okay's, both ways.**
- `fromEventStream(es)(owner)` and `fromSignal(s)(owner)`: a `Stream[O, Async]`, subscribed on its first pull, the
  events between pulls buffered, released with the `Owner` (a Laminar element's, or a `ManualOwner`); a `Signal`'s
  current value its first element.
- `toEventStream(s)`: an okay `Stream[O, Async]` as an `EventStream[O]` by `EventStream.fromCustomSource`: started
  (`asynchrony.start`) when Airstream starts it, its `Running` cancelled when Airstream stops it — so the stream runs
  exactly while something observes it. `fromProgram(p)`: one program the same way, its value or failure one event.
- `varState(v)(p)`: a `Var[S]` as the handler of `State % S`, in place: `get` is `v.now()`, `put` is `v.set`, so a page
  sees a program's state as it changes.
- `s.into(observer)`: a stream drained into an Airstream `Observer`.

**ui/laminar — programs in a page, tied to its elements.**
- `runOnMount(p)(done)`: a `Modifier` that starts the program when the element mounts and cancels it when it unmounts
  (okay-async's cancellation: what the program waits on is cancelled too); its result, a value or a failure, to `done`.
- `child.text <-- fromProgram(p)` and the like come from ui/airstream for free; this module adds the element-level
  words and a test of the lifecycle.

**ui/waypoint — routes as an effect.**
- `Navigation[P, X]`: `Push(page)`, `Replace(page)`, `Current` — operations a program performs, with Scala 2's
  `answer` functions (shared source).
- `router(r)(p)`: Waypoint's `Router[P]` as their handler: `pushState`, `replaceState`, `currentPageSignal.now()`.
- `pages(r)(owner)`: the current page as a `Stream[P, Async]` (ui/airstream's `fromSignal`).
- A test handler that records the pages asked for: a program that navigates, tested with no browser history.

**Build.** A JS-only cross project (`CrossProject(id, file(dir))(JSPlatform)`, defined only when `-Dplatforms` asks for
js), in `testJS` and `publishPages`. Tests under jsdom: `addSbtPlugin("org.scala-js" % "sbt-scalajs-env-jsdom-nodejs"
% "1.1.0")`, `jsEnv := new JSDOMNodeJSEnv(...)`; jsdom 29.1.1 is installed globally here (`/opt/homebrew/lib`), so the
env needs `NODE_PATH` (or a local `npm install jsdom`).

**Tests.**
- airstream: a stream crossed twice is the same elements; an `EventStream`'s events buffered between pulls; a `Signal`'s
  current value first; `toEventStream` starts once on the first observer and its program is cancelled when the last
  stops; `varState` sees every `put`.
- laminar (jsdom): mount starts, unmount cancels exactly once; a failure reaches `done`, not the console.
- waypoint (jsdom): `Push` changes `currentPageSignal` and the URL; `Current` answers it; the recording handler.
- Both compilers, as every module.

**Docs.** `docs/modules/ui.md`: the three modules, their versions, an example each. The blocks cannot run in docsCheck
(the JVM): mark them as excerpts of the tests, as the site does.

**Check first.** `EventStream.fromCustomSource`'s exact signature in 17.2.1; Waypoint 9.0.0's `Router` constructor
(routes, `getPageTitle`, `serializePage`, `deserializePage`, `popStateEvents`, `owner`) and whether it can run under
jsdom; how `AsyncStreams` holds its `ExecutionContext` (the browser's is the macrotask one), to do the same.
