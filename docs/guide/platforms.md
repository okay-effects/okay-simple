# Platforms and the build

## The JVM and Scala.js

The interface, the machine, `Free`, the suite, tagless, every module and interop with cats and ZIO are built for the
JVM and for Scala.js, by both compilers (Scala 3.9, Scala 2.13). One source for both platforms (`CrossType.Pure`);
the platform's own only where it must be — the `async` module's timer: a scheduler on the JVM, `setTimeout` on
Scala.js. tofu has no Scala.js build, so its interop is the JVM's.

In sbt, the JVM's project is named as the module (`machine`), Scala.js's with `JS` (`machineJS`).

## The build

```sh
sbt testAll          # both compilers, the JVM: every suite, the compatibility checks, the documentation's code
sbt testJS           # both compilers, Scala.js on Node.js: the suites, tagless, the modules, interop
sbt "+effects/publishLocal" "+machine/publishLocal"        # to use it: the interface and an implementation
sbt "bench/Jmh/run -f 2 -wi 5 -i 5"                        # the benchmark
```

`testJS` needs Node.js. The documentation's code blocks are compiled and their `assert`s run by `docsCheck`
(```scala blocks, Scala 3) and `docsCheck2` (```scala2 blocks, Scala 2.13).

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
