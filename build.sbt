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

/** the reference implementation, and the suites of the interface run on it */
lazy val free = (project in file("free")).dependsOn(core).settings(crossBuilt, name := "okay-simple-free")

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
lazy val compat2 = (project in file("compat/scala2")).dependsOn(free)
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
    Compile / unmanagedSourceDirectories := Seq((ThisBuild / baseDirectory).value / "free/src/main/scala"))
lazy val compat3on2 = (project in file("compat/scala3-on-2.13")).dependsOn(free213)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), libraryDependencies += munit,
    name := "okay-simple-compat-scala3-on-213", publish / skip := true,
    projectDependencies ~= (_.map(_.exclude("org.scala-lang", "scala-library"))),
    Test / unmanagedSourceDirectories := Seq((ThisBuild / baseDirectory).value / "free/src/test/scala"))

lazy val root = (project in file(".")).aggregate(core, free, compat2, compat3, core213, free213, compat3on2)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
addCommandAlias("testAll", "; ++3.9.0 free/test ; ++2.13.18 free/test ; ++3.9.0 ; compat2/test ; compat3/test ; compat3on2/test")
