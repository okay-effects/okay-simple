val scala3 = "3.9.0"
val scala2 = "2.13.18"
import sbtcrossproject.{CrossProject, CrossType}
import Platforms._

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
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test)

/** THE PLATFORMS BUILT: the JVM always; Scala.js and Scala Native when asked —
 * `sbt -Dplatforms=jvm,js,native ...`, or the environment's `OKAY_PLATFORMS=js,native` */
val platformsAsked: Set[String] =
  (sys.props.get("platforms") orElse sys.env.get("OKAY_PLATFORMS")).getOrElse("jvm").split(',').map(_.trim.toLowerCase).toSet + "jvm"
def platformsOf(can: Set[String]): Seq[sbtcrossproject.Platform] = Seq[sbtcrossproject.Platform](JVMPlatform) ++
  (if (platformsAsked("js") && can("js")) Seq(JSPlatform) else Nil) ++
  (if (platformsAsked("native") && can("native")) Seq(NativePlatform) else Nil)

/** ON EACH PLATFORM ASKED, of those it `can` be built for: one source (`CrossType.Pure`), the JVM's project named as the
 * module, Scala.js's `…JS`, Scala Native's `…Native` */
def crossBoth(id: String, dir: String, can: Set[String] = Set("js", "native")): CrossProject =
  CrossProject(id, file(dir))(platformsOf(can): _*).crossType(CrossType.Pure)
    .settings(crossBuilt).jvmConfigure(_.withId(id)).onJS(_.withId(id + "JS")).onNative(_.withId(id + "Native"))
/** munit, for the JVM-only projects */
val munit = "org.scalameta" %% "munit" % "1.2.3" % Test

/** THE INTERFACE: the types and the primitives, the standard effects written on them — no implementation */
lazy val effects = crossBoth("effects", "core/effects").settings(name := "okay-effects")

/** the fast implementation: a machine of an explicit stack, an operation finding its handler in one walk */
lazy val machine = crossBoth("machine", "core/machine").dependsOn(effects).settings(name := "okay-effects-machine")

/** THE INTERFACE'S SUITE, a library: `InterfaceSuite(fx)` and its programs, for any implementation — here's, and the
 * bridges' in their own repositories (okay-core's, okay-kernel's): `"dev.okay" %% "okay-effects-tck" % ... % Test` */
lazy val tck = crossBoth("tck", "tck").dependsOn(effects)
  .settings(name := "okay-effects-tck",
    libraryDependencies := libraryDependencies.value.filterNot(_.name == "munit") :+ ("org.scalameta" %%% "munit" % "1.2.3"))

/** the interface's suites, on each implementation, by both compilers */
lazy val suite = crossBoth("suite", "suite").dependsOn(machine, tck)
  .settings(name := "okay-effects-suite", publish / skip := true,
    Test / unmanagedSourceDirectories ++= Seq("src/test/impl", s"src/test/impl-${major(scalaVersion.value)}")
      .map(baseDirectory.value.getParentFile / _))

/** the interface on THE `Free` MONAD: Scala 3, a program a `Free` interpreted by its handlers */
lazy val free = crossBoth("free", "core/free").dependsOn(effects)
  .settings(name := "okay-effects-free")

/** the interface's suite on the `Free` monad */
lazy val suiteFree = crossBoth("suiteFree", "suite-free").dependsOn(free, tck)
  .settings(name := "okay-effects-suite-free", publish / skip := true)

/** EXAMPLES: how to write with the effects — code for both compilers, Scala 3's way, Scala 2's way — written over
 * `Effects` alone; run on the machine and on the `Free` monad by `OnMachine` and `OnFree`, their values checked on
 * each (`Checks`) */
lazy val examples = (project in file("examples")).dependsOn(machine.jvm, free.jvm)
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
lazy val interopCats = crossBoth("interopCats", "interop/cats").dependsOn(effects, machine, free % Test)
  .settings(name := "okay-effects-cats", libraryDependencies ++= Seq("org.typelevel" %%% "cats-core" % "2.13.0",
    // cats-effect 3's IO lowered to, in the checks: any cats monad is, the module asks no more
    "org.typelevel" %%% "cats-effect" % "3.7.1" % Test))

/** INTEROP WITH ZIO: a task performed in a program, a program lowered to a task */
lazy val interopZio = crossBoth("interopZio", "interop/zio").dependsOn(effects, machine, free % Test)
  .settings(name := "okay-effects-zio", libraryDependencies += "dev.zio" %%% "zio" % "2.1.14")
  // ZIO's runtime on Scala.js and Scala Native reads `java.time`: a port of it, for the checks
  .onJS(_.settings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0" % Test))
  .onNative(_.settings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0" % Test))

/** INTEROP WITH JAVA'S STREAMS AND ITERATORS: the stream module's streams to and from them. The JVM's: Scala.js has no
 * `java.util.stream` */
lazy val interopJavaStream = (project in file("interop/java-stream"))
  .dependsOn(effects.jvm, stream.jvm, machine.jvm, free.jvm % Test, tagless.jvm % Test)
  .settings(crossBuilt, name := "okay-effects-java-stream")

/** INTEROP WITH ZIO'S STREAMS: a stream of ZIO's tasks a `ZStream`, and back */
lazy val interopZioStream = crossBoth("interopZioStream", "interop/zio-stream")
  .dependsOn(interopZio, stream, machine, free % Test, tagless % Test)
  .settings(name := "okay-effects-zio-stream", libraryDependencies += "dev.zio" %%% "zio-streams" % "2.1.14")
  .onJS(_.settings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0" % Test))
  .onNative(_.settings(libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0" % Test))

/** INTEROP WITH FS2'S STREAMS: a stream of `F`'s programs (cats-effect's `IO`, any cats monad) an `fs2.Stream`, and
 * back */
lazy val interopFs2 = crossBoth("interopFs2", "interop/fs2")
  .dependsOn(interopCats, stream, machine, free % Test, tagless % Test)
  .settings(name := "okay-effects-fs2", libraryDependencies ++= Seq("co.fs2" %%% "fs2-core" % "3.14.0",
    "org.typelevel" %%% "cats-effect" % "3.7.1" % Test))

/** INTEROP WITH MONIX: its `Task` an effect, its `Observable` a stream and back. Monix 3 is of cats-effect 2: its own
 * module, no other interop beside it */
lazy val interopMonix = crossBoth("interopMonix", "interop/monix", can = Set("js"))
  .dependsOn(effects, stream, machine, free % Test, tagless % Test)
  .settings(name := "okay-effects-monix", libraryDependencies ++= Seq("io.monix" %%% "monix-reactive" % "3.5.2",
    "io.monix" %%% "monix-tail" % "3.5.2"))

/** INTEROP WITH TOFU: `WithLocal` of a `Reader`, `Errors` of an `Error`, over cats' monad of the programs */
lazy val interopTofu = (project in file("interop/tofu")).dependsOn(interopCats.jvm, machine.jvm, free.jvm % Test)
  // the JVM's only: tofu is not built for Scala.js
  .settings(crossBuilt, name := "okay-effects-tofu", libraryDependencies += "tf.tofu" %% "tofu-kernel" % "0.14.3")

/** TAGLESS: the interface's words over no implementation — a program a value, made once, run on the implementation
 * in the implicit context; each word that implementation's own. One per compiler: Scala 3's every word `inline` */
lazy val tagless = crossBoth("tagless", "core/tagless").dependsOn(effects, machine % Test, free % Test, tck % Test)
  .settings(name := "okay-effects-tagless")

/** MORE EFFECTS, each a module over the interface in `modules/`, both compilers, checked on the machine, on `Free`, on tagless */
def effectModule(dir: String, artifact: String) = crossBoth(dir, "modules/" + dir)
  // the machine its default: `import okay.effects.stream._` the module on it
  .dependsOn(effects, machine, free % Test, tagless % Test).settings(name := artifact)
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
/** asynchrony with no runtime of its own: a program waiting on callbacks, driven to a `Future`; timers per platform */
lazy val async = effectModule("async", "okay-effects-async").dependsOn(stream)

/** the two implementations side by side: `sbt "bench/Jmh/run -f 2 -wi 3 -i 5"` */
lazy val bench = (project in file("bench")).dependsOn(machine.jvm, free.jvm, tagless.jvm).enablePlugins(JmhPlugin)
  .settings(crossScalaVersions := Seq(scala3, scala2), name := "okay-effects-bench", publish / skip := true)

/** THE DOCUMENTATION'S CODE, CHECKED: each ```scala block of `docs/` (```scala2 for Scala 2.13), a page's blocks in
 * turn in one class, compiled; an instance made in a test, so a page's `assert`s run — a class, not an object: a
 * callback on another thread waits on no initialisation of it. `sbt docsCheck/test docsCheck2/test` */
def docBlocks(lang: String) = Def.task {
  val docs = (ThisBuild / baseDirectory).value / "docs"
  val out = (Test / sourceManaged).value / "docs"
  val Block = ("(?s)```" + lang + "\\n(.*?)```").r
  val pages = (docs ** "*.md").get.sortBy(_.getPath).flatMap { md =>
    val blocks = Block.findAllMatchIn(IO.read(md)).map(_.group(1)).toList
    if (blocks.isEmpty) None
    else Some(md.relativeTo(docs).get.getPath.replaceAll("[^A-Za-z0-9]", "_") -> blocks)
  }
  val objects = pages.map { case (name, blocks) =>
    val f = out / s"$name.scala"
    IO.write(f, s"package docs.check\n\nclass $name {\n" + blocks.mkString("\n") + "\n}\n")
    f
  }
  val suite = out / "DocsSuite.scala"
  IO.write(suite, "package docs.check\n\nclass DocsSuite extends munit.FunSuite {\n" +
    pages.map { case (name, _) => "  test(\"" + name + "\") { assert(new " + name + "().toString.nonEmpty) }\n" }.mkString + "}\n")
  objects :+ suite
}
lazy val docsCheck = (project in file("docs/check/scala-3"))
  .dependsOn(tck.jvm, machine.jvm, free.jvm, tagless.jvm, concurrent.jvm, resource.jvm, io.jvm, probability.jvm, parsing.jvm, logic.jvm,
    transaction.jvm, resilience.jvm, stream.jvm, async.jvm, interopCats.jvm, interopZio.jvm, interopTofu, interopJavaStream,
    interopZioStream.jvm, interopFs2.jvm)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), publish / skip := true, libraryDependencies += munit,
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7",
    scalacOptions ++= Seq("-deprecation", "-feature", "-Wconf:msg=unused:s"), Test / sourceGenerators += docBlocks("scala").taskValue)
/** monix's blocks, ```scala monix: monix 3 is of cats-effect 2, so a classpath of its own */
lazy val docsCheckMonix = (project in file("docs/check/monix"))
  .dependsOn(machine.jvm, stream.jvm, interopMonix.jvm)
  .settings(scalaVersion := scala3, crossScalaVersions := Seq(scala3), publish / skip := true, libraryDependencies += munit,
    scalacOptions ++= Seq("-deprecation", "-feature"), Test / sourceGenerators += docBlocks("scala monix").taskValue)
lazy val docsCheck2 = (project in file("docs/check/scala-2"))
  .dependsOn(machine.jvm, free.jvm, tagless.jvm, stream.jvm)
  .settings(scalaVersion := scala2, crossScalaVersions := Seq(scala2), publish / skip := true, libraryDependencies += munit,
    Test / sourceGenerators += docBlocks("scala2").taskValue)

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

/** every cross-built module, on each platform built */
lazy val crossBuiltModules: Seq[CrossProject] = Seq(effects, tck, machine, suite, free, suiteFree, tagless, concurrent, resource, io, probability, parsing, logic, transaction, resilience, stream, async, interopCats, interopZio, interopZioStream, interopFs2, interopMonix)
lazy val root = (project in file(".")).aggregate((Seq[ProjectReference](docsCheck, docsCheck2, docsCheckMonix, interopTofu, interopJavaStream, examples, free213, examples213, examplesMixed, compat2, compat3, effects213, machine213, compat3on2) ++ crossBuiltModules.flatMap(_.projects.values.map(p => p: ProjectReference))): _*)
  .settings(publish / skip := true, crossScalaVersions := Nil)

/** everything: the interface's suites by both compilers, then the two compilers in one application */
/** the same suites on Scala.js (Node.js) and on Scala Native, by both compilers — when built: `sbt -Dplatforms=jvm,js testJS`,
 * `sbt -Dplatforms=jvm,native testNative` */
addCommandAlias("testJS", if (platformsAsked("js")) "; ++3.9.0 suiteJS/test ; suiteFreeJS/test ; taglessJS/test ; concurrentJS/test ; resourceJS/test ; ioJS/test ; probabilityJS/test ; parsingJS/test ; logicJS/test ; transactionJS/test ; resilienceJS/test ; streamJS/test ; asyncJS/test ; interopCatsJS/test ; interopZioJS/test ; interopZioStreamJS/test ; interopFs2JS/test ; interopMonixJS/test ; ++2.13.18 suiteJS/test ; suiteFreeJS/test ; taglessJS/test ; concurrentJS/test ; resourceJS/test ; ioJS/test ; probabilityJS/test ; parsingJS/test ; logicJS/test ; transactionJS/test ; resilienceJS/test ; streamJS/test ; asyncJS/test ; interopCatsJS/test ; interopZioJS/test ; interopZioStreamJS/test ; interopFs2JS/test ; interopMonixJS/test ; ++3.9.0" else "; show platformsHelp")
addCommandAlias("testNative", if (platformsAsked("native")) "; ++3.9.0 suiteNative/test ; suiteFreeNative/test ; taglessNative/test ; concurrentNative/test ; resourceNative/test ; ioNative/test ; probabilityNative/test ; parsingNative/test ; logicNative/test ; transactionNative/test ; resilienceNative/test ; streamNative/test ; asyncNative/test ; interopCatsNative/test ; interopZioNative/test ; interopZioStreamNative/test ; interopFs2Native/test ; ++2.13.18 suiteNative/test ; suiteFreeNative/test ; taglessNative/test ; concurrentNative/test ; resourceNative/test ; ioNative/test ; probabilityNative/test ; parsingNative/test ; logicNative/test ; transactionNative/test ; resilienceNative/test ; streamNative/test ; asyncNative/test ; interopCatsNative/test ; interopZioNative/test ; interopZioStreamNative/test ; interopFs2Native/test ; ++3.9.0" else "; show platformsHelp")
lazy val platformsHelp = taskKey[String]("how to build for Scala.js and Scala Native")
ThisBuild / platformsHelp := "Scala.js and Scala Native are built when asked: sbt -Dplatforms=jvm,js,native (or OKAY_PLATFORMS=js,native)"
addCommandAlias("testAll", "; ++3.9.0 suite/test ; suiteFree/test ; tagless/test ; concurrent/test ; resource/test ; io/test ; probability/test ; parsing/test ; logic/test ; transaction/test ; resilience/test ; stream/test ; async/test ; examples/test ; interopCats/test ; interopZio/test ; interopTofu/test ; interopJavaStream/test ; interopZioStream/test ; interopFs2/test ; interopMonix/test ; ++2.13.18 suite/test ; suiteFree/test ; tagless/test ; concurrent/test ; resource/test ; io/test ; probability/test ; parsing/test ; logic/test ; transaction/test ; resilience/test ; stream/test ; async/test ; examples/test ; interopCats/test ; interopZio/test ; interopTofu/test ; interopJavaStream/test ; interopZioStream/test ; interopFs2/test ; interopMonix/test ; ++3.9.0 ; compat2/test ; compat3/test ; compat3on2/test ; examplesMixed/test ; docsCheck/test ; docsCheckMonix/test ; ++2.13.18 docsCheck2/test ; ++3.9.0")
