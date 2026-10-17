package okay.api

/** Scala 3's: an effect's operations by a match type on its name — Scala 3 selects no type from an abstract one. With
 * Scala 3's precedence a row needs no parentheses, `Int ! Reader % String + State % Int` */
trait Syntax { self: Effects =>
  /** the operation of the effect `E` answered by an `X`: `Op[State % Int, X]` is `State[Int, X]` */
  type Op[E <: Effect, X] = E match { case Name[f] => f[X] }
}
