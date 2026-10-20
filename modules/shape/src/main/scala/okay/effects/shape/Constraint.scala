package okay.effects.shape

/** a constraint on a value: DATA, so a face can show it before decoding, and a check, so the decoder can say no */
sealed trait Constraint
object Constraint {
  sealed trait OnText extends Constraint { def check(s: String): Option[String] }
  final case class MinLength(n: Int) extends OnText {
    def check(s: String): Option[String] = if (s.length < n) Some(s"at least $n characters") else None
  }
  final case class MaxLength(n: Int) extends OnText {
    def check(s: String): Option[String] = if (s.length > n) Some(s"at most $n characters") else None
  }
  /** `hint` is what a person reads: "an email address", not the regex */
  final case class Pattern(regex: String, hint: String) extends OnText {
    @transient private lazy val compiled = regex.r
    def check(s: String): Option[String] = if (compiled.matches(s)) None else Some(s"not $hint")
  }

  sealed trait OnNumber extends Constraint { def check(n: BigDecimal): Option[String] }
  final case class Min(n: BigDecimal) extends OnNumber {
    def check(x: BigDecimal): Option[String] = if (x < n) Some(s"at least $n") else None
  }
  final case class Max(n: BigDecimal) extends OnNumber {
    def check(x: BigDecimal): Option[String] = if (x > n) Some(s"at most $n") else None
  }
}
