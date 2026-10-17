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
                     // the machine's classes are members of one object's trait: no outer reference to tell apart
                     else Seq("-deprecation", "-feature", "-Xlint", "-Wconf:msg=outer reference in this type test:s")),
  libraryDependencies += munit)

/** THE INTERFACE: the types and the primitives, the standard effects written on them — no implementation */
lazy val effects = (project in file("effects")).settings(crossBuilt, name := "okay-effects")

/** the fast implementation: a machine of an explicit stack, an operation finding its handler in one walk */
lazy val machine = (project in file("machine")).dependsOn(effects).settings(crossBuilt, name := "okay-effects-machine")

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
lazy val free = (project in file("free")).dependsOn(effects)
  .settings(crossBuilt, name := "okay-effects-free")

/** the interface's suite on the `Free` monad */
lazy val suiteFree = (project in file("suite-free")).dependsOn(free, tck)
  .settings(crossBuilt, name := "okay-effects-suite-free", publish / skip := true)

/** EXAMPLES: how to write with the effects — code for both compilers, Scala 3's way, Scala 2's way — over any
 * implementation, of none: the interface alone. Their values checked by suites over `Effects` (`Checks`), run by the
 * runners below */
lazy val examples = (project in file("examples")).dependsOn(effects)
  .settings(crossBuilt, name := "okay-effects-examples", publish / skip := true)

/** the examples ON THE MACHINE: `sbt examplesOnMachine/run`, and their checks */
lazy val examplesOnMachine = (project in file("examples/on-machine"))
  .dependsOn(examples % "compile->compile;test->test", machine)
  .settings(crossBuilt, name := "okay-effects-examples-on-machine", publish / skip := true)

/** the examples ON THE `Free` MONAD: `sbt examplesOnFree/run`, and their checks */
lazy val examplesOnFree = (project in file("examples/on-free"))
  .dependsOn(examples % "compile->compile;test->test", free)
  .settings(crossBuilt, name := "okay-effects-examples-on-free", publish / skip := true)

/** the `Free` monad as Scala 2.13 builds it, on the interface's 2.13 build */
lazy val free213 = (project in file("compat/free-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-free-213",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("free/src/main/scala", "free/src/main/scala-2")
      .map((ThisBuild / baseDirectory).value / _))

/** the examples as Scala 2.13 builds them, on the interface's 2.13 build: Scala 2 code for the mixed application */
lazy val examples213 = (project in file("examples/scala-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-examples-213",
    publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("examples/src/main/scala", "examples/src/main/scala-2")
      .map((ThisBuild / baseDirectory).value / _))

/** ONE APPLICATION OF TWO COMPILERS: Scala 3 code beside the Scala 2.13 examples, in one row, one run — on the
 * machine and on the `Free` monad, their 2.13 builds */
lazy val examplesMixed = (project in file("examples/mixed")).dependsOn(examples213, machine213, free213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-examples-mixed", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))))

/** the two implementations side by side: `sbt "bench/Jmh/run -f 2 -wi 3 -i 5"` */
lazy val bench = (project in file("bench")).dependsOn(machine, free).enablePlugins(JmhPlugin)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-effects-bench", publish / skip := true)

/**
 * COMPATIBILITY. One application has one build of the interface. The interface is one source; how a row is written
 * is each build's own — Scala 3's `%` a type lambda, Scala 2's a name with its constructor `#L`, which Scala 2 code
 * needs — so an application with Scala 2 code in it takes the Scala 2.13 build, which Scala 3 reads. Below:
 * the interface and its implementations as Scala 2.13 builds them; the interface's suite compiled by Scala 3 against
 * that build; an effect, its handler and a program written in Scala 2.13, used by Scala 3 code beside its own
 */
lazy val effects213 = (project in file("compat/effects-2.13"))
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-213", publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("effects/src/main/scala", "effects/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
lazy val machine213 = (project in file("compat/machine-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-effects-machine-213", publish / skip := true,
    scalacOptions += "-Wconf:msg=outer reference in this type test:s",
    Compile / unmanagedSourceDirectories := Seq("machine/src/main/scala", "machine/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
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

lazy val root = (project in file(".")).aggregate(effects, tck, machine, suite, free, suiteFree, examples, examplesOnMachine, examplesOnFree, free213, examples213, examplesMixed, compat2, compat3, effects213, machine213, compat3on2)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
addCommandAlias("testAll", "; ++3.9.0 suite/test ; suiteFree/test ; examplesOnMachine/test ; examplesOnFree/test ; ++2.13.18 suite/test ; suiteFree/test ; examplesOnMachine/test ; examplesOnFree/test ; ++3.9.0 ; compat2/test ; compat3/test ; compat3on2/test ; examplesMixed/test")
