package okay.effects.shape

import okay.effects.json.Json

/**
 * READING A SHAPE IS AN EFFECT: the fields a shape names are asked for, one at a time, down the tree, and WHERE they
 * come from is the handler's — a `Json` (`fromJson`, which is `decode`), a command line, another encoding, a PERSON
 * asked field by field who may be asked again when an answer did not decode (`Retry`). Every operation carries the
 * path and the erased shape of what is asked, with its labels and constraints, so a handler can show them.
 */
sealed trait Fields[X]
object Fields {
  /** a value in one piece at `path` — a primitive, or one refined or described; `None` when there is none */
  final case class Get[X](path: List[String], shape: Schema, answer: Option[Json] => X) extends Fields[X]
  /** the value at `path` did not decode: the handler may give another (a person, asked again) or not (`None`: a wire) */
  final case class Retry[X](path: List[String], shape: Schema, problems: Problems, answer: Option[Json] => X) extends Fields[X]
  /** what is at `path`: nothing, something of the shape's kind, or something else (a text where a record was asked) */
  final case class Has[X](path: List[String], shape: Schema, answer: Presence => X) extends Fields[X]
  sealed trait Presence
  object Presence {
    case object Absent extends Presence
    case object Present extends Presence
    /** something of another kind, named as `Json.kind` names it */
    final case class Other(kind: String) extends Presence
  }
  /** how many elements the list at `path` has; `None` when there is no list */
  final case class Size[X](path: List[String], shape: Schema, answer: Option[Int] => X) extends Fields[X]
  /** which case of a sum is at `path`, by name; `None` when none is named */
  final case class Which[X](path: List[String], cases: List[String], answer: Option[String] => X) extends Fields[X]
}
