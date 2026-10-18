# Platforms and the build

## The JVM, Scala.js, Scala Native

The interface, the machine, `Free`, the suite, tagless, every module and interop with cats, ZIO, fs2 and ZIO's streams
are built for the JVM, Scala.js and Scala Native, by both compilers (Scala 3.3 LTS, Scala 2.13; kyo's interop on Scala 3.9, as kyo is); monix for the JVM and
Scala.js (it has no Scala Native build); tofu and Java's streams for the JVM. One source for every platform
(`CrossType.Pure`); the platform's own only where it must be — the `async` module's timer and execution context: a
scheduler and the global context on the JVM and Scala Native, `setTimeout` and the microtask queue on Scala.js.

By default only the JVM is built. Scala.js and Scala Native are built when asked:

```sh
sbt testAll                                        # the JVM: every suite, the compatibility checks, the documentation's code
sbt -Dplatforms=jvm,js testJS                      # and Scala.js, on Node.js
sbt -Dplatforms=jvm,native testNative              # and Scala Native
sbt -Dplatforms=jvm,js,native "+effectsJS/publishLocal" "+effectsNative/publishLocal"
OKAY_PLATFORMS=js,native sbt ...                   # the same, from the environment
```

In sbt, the JVM's project is named as the module (`machine`), Scala.js's with `JS` (`machineJS`), Scala Native's with
`Native` (`machineNative`). `testJS` needs Node.js; `testNative` clang (Scala Native 0.5, its own garbage collector).

## The build

```sh
sbt "+effects/publishLocal" "+machine/publishLocal"        # to use it: the interface and an implementation
sbt "bench/Jmh/run -f 2 -wi 5 -i 5"                        # the benchmark
```

The documentation's code blocks are compiled and their `assert`s run by `docsCheck` (```scala blocks, Scala 3),
`docsCheck2` (```scala2 blocks, Scala 2.13) and `docsCheckMonix` (```scala monix blocks: monix 3 is of cats-effect 2,
so a classpath of its own) — part of `testAll`.

## Layout

| Directory | |
|---|---|
| `core/effects` | the interface, the standard effects, delimited control, `Places` |
| `core/machine`, `core/free`, `core/tagless` | the implementations |
| `tck`, `suite`, `suite-free` | the interface's suite, and it on the machine and `Free` |
| `modules/*` | more effects, each a module |
| `interop/*` | cats, ZIO, tofu |
| `examples/` | examples, and an application of both compilers |
| `compat/` | the Scala 2.13 build read by Scala 3 |
| `bench/`, `bench.d/` | JMH, and its saved runs |
| `docs/` | this documentation |
