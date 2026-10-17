package okay.std

/** THE STANDARD EFFECTS' DEFINITIONS, apart, the same for both compilers: in this object, so that another
 * `okay.std`'s effects of the same names do not clash with them. `Std` (each compiler its own) gives them by name */
object Standard {
  /** the standard effects: each a GADT of its operations. An operation says how its answer is made, `answer`, so a
   * handler's clause needs no GADT inference — Scala 2 has none for a method's type parameter — and no cast */
  sealed trait State[S, X]
  object State {
    final case class Get[S, X](answer: S => X) extends State[S, X]
    final case class Put[S, X](s: S, answer: Unit => X) extends State[S, X]
  }

  sealed trait Reader[E, X]
  object Reader {
    final case class Ask[E, X](answer: E => X) extends Reader[E, X]
  }

  sealed trait Writer[W, X]
  object Writer {
    final case class Tell[W, X](w: W, answer: Unit => X) extends Writer[W, X]
  }

  sealed trait Error[E, X]
  object Error {
    final case class Raise[E, X](e: E) extends Error[E, X]
  }

  /** an effect of no parameter: `Choose` in a Scala 3 row, `*[Choose]` in a Scala 2 one */
  sealed trait Choose[X]
  object Choose {
    final case class Among[X](xs: List[X]) extends Choose[X]
  }
}
