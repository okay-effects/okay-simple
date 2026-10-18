import sbt._
import sbtcrossproject.CrossProject
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.JSPlatform
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport.NativePlatform

/** a platform's settings, where it is built: Scala.js and Scala Native are built when asked */
object Platforms {
  implicit class OnPlatforms(val cp: CrossProject) extends AnyVal {
    def onJS(f: Project => Project): CrossProject = if (cp.projects.contains(JSPlatform)) cp.configurePlatform(JSPlatform)(f) else cp
    def onNative(f: Project => Project): CrossProject =
      if (cp.projects.contains(NativePlatform)) cp.configurePlatform(NativePlatform)(f) else cp
  }
}
