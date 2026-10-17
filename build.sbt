val scala3 = "3.9.0"
val scala2 = "2.13.18"
val munit = "org.scalameta" %% "munit" % "1.2.3" % Test

ThisBuild / organization := "dev.okay"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3

/** the compiler's major version, for the sources only its own */
def major(v: String): String = if (v.startsWith("3")) "3" else "2"
/** the interface's suite: its shared part, and the programs written in the compiler's interface */
def suiteOf(v: String): Seq[String] = Seq("tck/src/main/scala", s"tck/src/main/scala-${major(v)}")

/** one source for both compilers, and each its own convenience in scala-2/ and scala-3/ */
def crossBuilt: Seq[Def.Setting[?]] = Seq(
  crossScalaVersions := Seq(scala3, scala2),
  scalacOptions ++= (if (scalaVersion.value.startsWith("3")) Seq("-deprecation", "-feature")
                     else Seq("-deprecation", "-feature", "-Xlint")),
  libraryDependencies += munit)

/** THE INTERFACE: the types and the primitives, the standard effects written on them — no implementation */
lazy val effects = (project in file("core/effects")).settings(crossBuilt, name := "okay-effects")

/** the fast implementation: a machine of an explicit stack, an operation finding its handler in one walk */
lazy val machine = (project in file("core/machine")).dependsOn(effects).settings(crossBuilt, name := "okay-effects-machine")

/** THE INTERFACE'S SUITE, a library: `InterfaceSuite(fx)` and its programs, for any implementation — here's, and the
 * bridges' in their own repositories (okay-core's, okay-kernel's): `"dev.okay" %% "okay-effects-tck" % ... % Test` */
lazy val tck = (project in file("tck")).dependsOn(effects)
  .settings(crossBuilt, name := "okay-effects-tck",
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "munit") :+ ("org.scalameta" %% "munit" % "1.2.3"))

/** the interface's suites, on each implementation, by both compilers */
lazy val suite = (project in file("suite")).dependsOn(machine, tck)
  .settings(crossBuilt, name := "okay-effects-suite", publish / skip := true,
    Test / unmanagedSourceDirectories ++= Seq("src/test/impl", s"src/test/impl-${major(scalaVersion.value)}").map(baseDirectory.value / _))

/** the interface on THE `Free` MONAD: Scala 3, a program a `Free` interpreted by its handlers */
lazy val free = (project in file("core/free")).dependsOn(effects)
  .settings(crossBuilt, name := "okay-effects-free")

/** the interface's suite on the `Free` monad */
lazy val suiteFree = (project in file("suite-free")).dependsOn(free, tck)
  .settings(crossBuilt, name := "okay-effects-suite-free", publish / skip := true)

/** EXAMPLES: how to write with the effects — code for both compilers, Scala 3's way, Scala 2's way — written over
 * `Effects` alone; run on the machine and on the `Free` monad by `OnMachine` and `OnFree`, their values checked on
 * each (`Checks`) */
lazy val examples = (project in file("examples")).dependsOn(machine, free)
  .settings(crossBuilt, name := "okay-effects-examples", publish / skip := true)

/** the `Free` monad as Scala 2.13 builds it, on the interface's 2.13 build */
lazy val free213 = (project in file("compat/free-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-free-213",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("core/free/src/main/scala", "core/free/src/main/scala-2")
      .map((ThisBuild / baseDirectory).value / _))

/** the examples as Scala 2.13 builds them, on the interface's 2.13 build: Scala 2 code for the mixed application */
lazy val examples213 = (project in file("examples/scala-2.13")).dependsOn(machine213, free213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-examples-213",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("examples/src/main/scala", "examples/src/main/scala-2")
      .map((ThisBuild / baseDirectory).value / _))

/** ONE APPLICATION OF TWO COMPILERS: Scala 3 code beside the Scala 2.13 examples, in one row, one run — on the
 * machine and on the `Free` monad, their 2.13 builds */
lazy val examplesMixed = (project in file("examples/mixed")).dependsOn(examples213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-examples-mixed", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))))

/** INTEROP WITH CATS: cats' `Monad` of the programs, a cats monad's programs lifted in and a program lowered to one —
 * over any implementation, both compilers */
lazy val interopCats = (project in file("interop/cats")).dependsOn(effects, machine % Test, free % Test)
  .settings(crossBuilt, name := "okay-effects-cats", libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "2.13.0",
    // cats-effect 3's IO lowered to, in the checks: any cats monad is, the module asks no more
    "org.typelevel" %% "cats-effect" % "3.5.7" % Test))

/** INTEROP WITH ZIO: a task performed in a program, a program lowered to a task */
lazy val interopZio = (project in file("interop/zio")).dependsOn(effects, machine % Test, free % Test)
  .settings(crossBuilt, name := "okay-effects-zio", libraryDependencies += "dev.zio" %% "zio" % "2.1.14")

/** INTEROP WITH TOFU: `WithLocal` of a `Reader`, `Errors` of an `Error`, over cats' monad of the programs */
lazy val interopTofu = (project in file("interop/tofu")).dependsOn(interopCats, machine % Test, free % Test)
  .settings(crossBuilt, name := "okay-effects-tofu", libraryDependencies += "tf.tofu" %% "tofu-kernel" % "0.14.3")

/** TAGLESS: the interface's words over no implementation — a program a value, made once, run on the implementation
 * in the implicit context; each word that implementation's own. One per compiler: Scala 3's every word `inline` */
lazy val tagless = (project in file("tagless")).dependsOn(effects, machine % Test, free % Test, tck % Test)
  .settings(crossBuilt, name := "okay-effects-tagless")

/** MORE EFFECTS, each a module over the interface in `modules/`, both compilers, checked on the machine, on `Free`, on tagless */
def effectModule(dir: String, artifact: String): Project = Project(dir, file("modules/" + dir)).dependsOn(effects, machine % Test, free % Test, tagless % Test)
  .settings(crossBuilt, name := artifact)
/** cooperative threads in turns, a channel between them */
lazy val concurrent = effectModule("concurrent", "okay-effects-concurrent")
/** resources released at a scope's end */
lazy val resource = effectModule("resource", "okay-effects-resource")
/** the console, the clock, the environment: the world's handlers and pure ones */
lazy val io = effectModule("io", "okay-effects-io")
/** random choices: the exact distribution, or sampled */
lazy val probability = effectModule("probability", "okay-effects-probability")
/** parsers of the input as a state and the alternatives as choices */
lazy val parsing = effectModule("parsing", "okay-effects-parsing")
/** logic variables, unification, a fair search */
lazy val logic = effectModule("logic", "okay-effects-logic")
/** transactions on a store: a log written at the end, dropped on an error */
lazy val transaction = effectModule("transaction", "okay-effects-transaction")
/** retries and fallbacks */
lazy val resilience = effectModule("resilience", "okay-effects-resilience")
/** lazy streams pulled by effects; to and from generators */
lazy val stream = effectModule("stream", "okay-effects-stream")

/** the two implementations side by side: `sbt "bench/Jmh/run -f 2 -wi 3 -i 5"` */
lazy val bench = (project in file("bench")).dependsOn(machine, free, tagless).enablePlugins(JmhPlugin)
  .settings(crossScalaVersions := Seq(scala3, scala2), name := "okay-effects-bench", publish / skip := true)

/**
 * COMPATIBILITY. One application has one build of the interface. The interface is one source; how a row is written
 * is each build's own — Scala 3's `%` a type lambda, Scala 2's a name with its constructor `#L`, which Scala 2 code
 * needs — so an application with Scala 2 code in it takes the Scala 2.13 build, which Scala 3 reads. Below:
 * the interface and its implementations as Scala 2.13 builds them; the interface's suite compiled by Scala 3 against
 * that build; an effect, its handler and a program written in Scala 2.13, used by Scala 3 code beside its own
 */
lazy val effects213 = (project in file("compat/effects-2.13"))
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-213", publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("core/effects/src/main/scala", "core/effects/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
lazy val machine213 = (project in file("compat/machine-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-machine-213", publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("core/machine/src/main/scala", "core/machine/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
lazy val compat3on2 = (project in file("compat/scala3-on-2.13")).dependsOn(machine213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-compat-scala3-on-213", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))),
    Test / unmanagedSourceDirectories := (suiteOf(scala2) ++ Seq("suite/src/test/impl", "suite/src/test/impl-2")).map((ThisBuild / baseDirectory).value / _))

/** an effect, its handler and a program written in Scala 2.13, on the Scala 2.13 build */
lazy val compat2 = (project in file("compat/scala2")).dependsOn(machine213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), libraryDependencies += munit,
    name := "okay-effects-compat-scala2", publish / skip := true)

/** Scala 3 code using the Scala 2.13 library's effect and program beside its own, in one row */
lazy val compat3 = (project in file("compat/scala3")).dependsOn(compat2)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-compat-scala3", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))))

lazy val root = (project in file(".")).aggregate(effects, tck, machine, suite, free, suiteFree, tagless, concurrent, resource, io, probability, parsing, logic, transaction, resilience, stream, interopCats, interopZio, interopTofu, examples, free213, examples213, examplesMixed, compat2, compat3, effects213, machine213, compat3on2)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
addCommandAlias("testAll", "; ++3.9.0 suite/test ; suiteFree/test ; tagless/test ; concurrent/test ; resource/test ; io/test ; probability/test ; parsing/test ; logic/test ; transaction/test ; resilience/test ; stream/test ; examples/test ; interopCats/test ; interopZio/test ; interopTofu/test ; ++2.13.18 suite/test ; suiteFree/test ; tagless/test ; concurrent/test ; resource/test ; io/test ; probability/test ; parsing/test ; logic/test ; transaction/test ; resilience/test ; stream/test ; examples/test ; interopCats/test ; interopZio/test ; interopTofu/test ; ++3.9.0 ; compat2/test ; compat3/test ; compat3on2/test ; examplesMixed/test")
