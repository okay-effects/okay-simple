val scala3 = "3.9.0"
val scala2 = "2.13.18"
val munit = "org.scalameta" %% "munit" % "1.2.3" % Test

ThisBuild / organization := "dev.okay"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3

/** the compiler's major version, for the sources only its own */
def major(v: String): String = if (v.startsWith("3")) "3" else "2"
/** the interface's suite: its shared part, and the programs written in the compiler's interface */
def suiteOf(v: String): Seq[String] = Seq("suite/src/test/scala", s"suite/src/test/scala-${major(v)}")

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
lazy val simple = (project in file("simple")).dependsOn(effects).settings(crossBuilt, name := "okay-simple")

/** the interface's suites, on each implementation, by both compilers */
lazy val suite = (project in file("suite")).dependsOn(simple)
  .settings(crossBuilt, name := "okay-effects-suite", publish / skip := true,
    Test / unmanagedSourceDirectories ++= Seq("src/test/impl", s"src/test/impl-${major(scalaVersion.value)}").map(baseDirectory.value / _))

/** okay-simple's interface on okay-core's machine: Scala 3, okay-core's artifact (`sbt publishLocal` in okay-core) */
lazy val core = (project in file("core")).dependsOn(effects)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-effects-core",
    resolvers += Resolver.mavenLocal,
    libraryDependencies ++= Seq("okay-core" %% "okay-core" % "0.1.0-SNAPSHOT", munit))

/** okay-simple's interface on okay-kernel's protocol: any implementation of it, given its `Effects` */
lazy val kernel = (project in file("kernel")).dependsOn(effects)
  .settings(crossBuilt, name := "okay-effects-kernel", resolvers += Resolver.mavenLocal,
    libraryDependencies += "dev.okay" % "okay-kernel-api" % "0.2.0-SNAPSHOT")

/** the interface's suite through the protocol: okay-kernel's reference machine and okay-java on both compilers,
 * okay-core's bridge on Scala 3 */
lazy val suiteKernel = (project in file("suite-kernel")).dependsOn(kernel)
  .settings(crossBuilt, name := "okay-effects-suite-kernel", publish / skip := true, resolvers += Resolver.mavenLocal,
    // the protocol's Java implementations on the classpath: one class loader, not sbt's layers cached across ++
    Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
    libraryDependencies ++= Seq(
      "dev.okay" %% "okay-kernel-steps" % "0.2.0-SNAPSHOT" % Test,
      "dev.okay" % "okay-java" % "0.2.0-SNAPSHOT" % Test) ++
      (if (scalaVersion.value.startsWith("3")) Seq("okay-core" %% "okay-core" % "0.1.0-SNAPSHOT" % Test) else Nil),
    Test / unmanagedSourceDirectories ++= suiteOf(scalaVersion.value).map((ThisBuild / baseDirectory).value / _))

/** the interface's suite on okay-core's machine */
lazy val suiteCore = (project in file("suite-core")).dependsOn(core)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-effects-suite-core", publish / skip := true,
    libraryDependencies += munit,
    Test / unmanagedSourceDirectories ++= suiteOf(scala3).map((ThisBuild / baseDirectory).value / _))

/** the two implementations side by side: `sbt "bench/Jmh/run -f 2 -wi 3 -i 5"` */
lazy val bench = (project in file("bench")).dependsOn(simple, core, kernel).enablePlugins(JmhPlugin)
  .settings(crossScalaVersions := Seq(scala3), name := "okay-effects-bench", publish / skip := true,
    resolvers += Resolver.mavenLocal, libraryDependencies ++= Seq(
      "dev.okay" %% "okay-kernel-steps" % "0.2.0-SNAPSHOT", "dev.okay" % "okay-java" % "0.2.0-SNAPSHOT"))

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
lazy val simple213 = (project in file("compat/simple-2.13")).dependsOn(effects213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), name := "okay-simple-213", publish / skip := true,
    scalacOptions += "-Wconf:msg=outer reference in this type test:s",
    Compile / unmanagedSourceDirectories := Seq("simple/src/main/scala", "simple/src/main/scala-2").map((ThisBuild / baseDirectory).value / _))
lazy val compat3on2 = (project in file("compat/scala3-on-2.13")).dependsOn(simple213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-compat-scala3-on-213", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))),
    Test / unmanagedSourceDirectories := (suiteOf(scala2) ++ Seq("suite/src/test/impl", "suite/src/test/impl-2")).map((ThisBuild / baseDirectory).value / _))

/** an effect, its handler and a program written in Scala 2.13, on the Scala 2.13 build */
lazy val compat2 = (project in file("compat/scala2")).dependsOn(simple213)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), libraryDependencies += munit,
    name := "okay-effects-compat-scala2", publish / skip := true)

/** Scala 3 code using the Scala 2.13 library's effect and program beside its own, in one row */
lazy val compat3 = (project in file("compat/scala3")).dependsOn(compat2)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-effects-compat-scala3", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))))

lazy val root = (project in file(".")).aggregate(effects, simple, suite, core, suiteCore, kernel, suiteKernel, compat2, compat3, effects213, simple213, compat3on2)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
addCommandAlias("testAll", "; ++3.9.0 suite/test ; suiteKernel/test ; ++2.13.18 suite/test ; suiteKernel/test ; ++3.9.0 ; suiteCore/test ; compat2/test ; compat3/test ; compat3on2/test")
