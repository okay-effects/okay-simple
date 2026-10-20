package okay.effects.world

/** a line typed without echo, where the JVM has a console of its own (`System.console`); a plain line where it has
 * not — under sbt, in a pipe */
private[world] object Secret {
  def read(): Option[String] = {
    val c = System.console()
    if (c == null) Option(scala.io.StdIn.readLine())
    else Option(c.readPassword()).map(new String(_))
  }
}
