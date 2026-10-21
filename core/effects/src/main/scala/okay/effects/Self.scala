package okay.effects

/**
 * AN IMPLEMENTATION CARRYING ITSELF AS AN IMPLICIT, of its own singleton type: `import Machine._` brings the words AND
 * `Machine` as the implicit implementation, so `new Faces()`, `run(p)`, `new Https()` need nothing more said, and what is
 * inferred is `Machine.type` exactly — the one `!` everything over the machine shares. Mixed into the implementations'
 * VALUES (`Machine`, `Free`), not into `Effects`: so `import fx._` over an `fx: F <: Effects` of one's own, a class's
 * parameter, brings no second implicit beside it. Mind Scala 2 when an implicit `Runner` of your own shares a scope
 * with an imported implementation's words: Scala 2 takes the more specific type, the imported one's; Scala 3 the nearer.
 */
trait Self { this: Effects =>
  implicit val self: this.type = this
}
