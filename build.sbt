val scala3 = "3.9.0"
val scala2 = "2.13.18"
val munit = "org.scalameta" %% "munit" % "1.2.3" % Test

ThisBuild / organization := "dev.okay"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3

/** one source for both compilers, and each its own convenience in scala-2/ and scala-3/ */
def crossBuilt: Seq[Def.Setting[?]] = Seq(
  crossScalaVersions := Seq(scala3, scala2),
  scalacOptions ++= (if (scalaVersion.value.startsWith("3")) Seq("-deprecation", "-feature")
                     else Seq("-deprecation", "-feature", "-Xlint")),
  libraryDependencies += munit)

/** THE INTERFACE: the types and the primitives, the standard effects written on them — no implementation */
lazy val core = (project in file("core")).settings(crossBuilt, name := "okay-simple")

/** the reference implementation: a free monad, an operation passed on by each handler it crosses */
lazy val free = (project in file("free")).dependsOn(core).settings(crossBuilt, name := "okay-simple-free")

/** the fast implementation: a machine of an explicit stack, an operation finding its handler in one walk */
lazy val machine = (project in file("machine")).dependsOn(core).settings(crossBuilt, name := "okay-simple-machine")

/** the interface's suites, on each implementation, by both compilers */
lazy val suite = (project in file("suite")).dependsOn(free, machine)
  .settings(crossBuilt, name := "okay-simple-suite", publish / skip := true,
    Test / unmanagedSourceDirectories += baseDirectory.value / "src/test/impl")

/** okay-simple's interface on okay-core's machine: Scala 3, okay-core's artifact (`sbt publishLocal` in okay-core) */
lazy val okaycore = (project in file("okaycore")).dependsOn(core)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-simple-okay-core",
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq("okay-core" %% "okay-core" % "0.1.0-SNAPSHOT", munit))

/** the interface's suite on okay-core's machine */
lazy val suiteOkaycore = (project in file("suite-okaycore")).dependsOn(okaycore)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-simple-suite-okay-core", publish / skip := true,
    libraryDependencies += munit,
    Test / unmanagedSourceDirectories += (ThisBuild / baseDirectory).value / "suite/src/test/scala")

/** the two implementations side by side: `sbt "bench/Jmh/run -f 2 -wi 3 -i 5"` */
lazy val bench = (project in file("bench")).dependsOn(free, machine, okaycore).enablePlugins(JmhPlugin)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-simple-bench", publish / skip := true)

/** Scala 3's standard library for a Scala 2.13 project, resolved here and put BEHIND 2.13's by hand (okay-core's
 * finding: 2.13's first, to compile — Scala 3's carries TASTy, not Scala 2 pickles — and Scala 3's after it, to run) */
lazy val Scala3Stdlib = config("scala3Stdlib").hide

/** a project compiled by scalac 2.13 against the core as Scala 3 built it (`-Ytasty-reader`) */
def scala2ReadingScala3: Seq[Def.Setting[?]] = Seq(
  scalaVersion := scala2,
  crossScalaVersions := Seq(scala2),
  projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library").exclude("org.scala-lang", "scala3-library_3"))),
  scalacOptions := Seq("-Ytasty-reader", "-deprecation", "-feature", "-Xlint"),
  ivyConfigurations += Scala3Stdlib,
  libraryDependencies ++= Seq(
    munit,
    "org.scala-lang" % "scala-library" % scala3 % Scala3Stdlib,
    "org.scala-lang" % "scala3-library_3" % scala3 % Scala3Stdlib)
) ++ Seq(Compile, Runtime, Test).flatMap(c => Seq(
  c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value),
  c / dependencyClasspathAsJars ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value)))

/** COMPATIBILITY, 1: an effect, its handler and a program written in Scala 2.13, on the interface Scala 3 built */
lazy val compat2 = (project in file("compat/scala2")).dependsOn(free, machine)
  .settings(scala2ReadingScala3, name := "okay-simple-compat-scala2", publish / skip := true)

/** COMPATIBILITY, 2: Scala 3 code using the Scala 2.13 library's effect and program beside its own */
lazy val compat3 = (project in file("compat/scala3")).dependsOn(compat2)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-simple-compat-scala3", publish / skip := true)

/** COMPATIBILITY, 3: the other way — the interface and the implementation as Scala 2.13 builds them, and the
 * interface's suite compiled by Scala 3 against that build */
lazy val core213 = (project in file("compat/core-2.13"))
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-simple-core-213", publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("core/src/main/scala", "core/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
lazy val free213 = (project in file("compat/free-2.13")).dependsOn(core213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-simple-free-213", publish / skip := true,
    Compile / unmanagedSourceDirectories := Seq("free/src/main/scala", "machine/src/main/scala").map((ThisBuild / baseDirectory).value / _))
lazy val compat3on2 = (project in file("compat/scala3-on-2.13")).dependsOn(free213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-simple-compat-scala3-on-213", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))),
    Test / unmanagedSourceDirectories := Seq("suite/src/test/scala", "suite/src/test/impl").map((ThisBuild / baseDirectory).value / _))

lazy val root = (project in file(".")).aggregate(core, free, machine, suite, okaycore, suiteOkaycore, compat2, compat3, core213, free213, compat3on2)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
addCommandAlias("testAll", "; ++3.9.0 suite/test ; ++2.13.18 suite/test ; ++3.9.0 ; suiteOkaycore/test ; compat2/test ; compat3/test ; compat3on2/test")
