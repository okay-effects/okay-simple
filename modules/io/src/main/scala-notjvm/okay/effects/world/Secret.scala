package okay.effects.world

/** Scala.js and Scala Native have no console that hides the typing: a plain line */
private[world] object Secret {
  def read(): Option[String] = Option(scala.io.StdIn.readLine())
}
