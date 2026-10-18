addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
// the interface, its implementations, tagless, the modules and interop cross-built: the JVM always, Scala.js and Scala
// Native when asked (`sbt -Dplatforms=jvm,js,native`) — okay-core's versions
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.4.0")
