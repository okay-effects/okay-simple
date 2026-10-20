package okay.effects.shape

/** what a decoder says: WHERE (field names and indices, root first; empty at the root) and what */
final case class Problem(path: List[String], message: String) {
  def render: String = if (path.isEmpty) message else path.mkString(".") + ": " + message
}
object Problem {
  def render(ps: Problems): String = ps.map(_.render).mkString("; ")
}
