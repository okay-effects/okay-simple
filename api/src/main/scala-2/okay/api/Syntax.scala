package okay.api

/** Scala 2's: an effect's operations its name's member, `E#Ops[X]` — Scala 2 selects a type from an abstract one */
trait Syntax { self: Effects =>
  /** the operation of the effect `E` answered by an `X`: `Op[State % Int, X]` is `State[Int, X]` */
  type Op[E <: Effect, X] = E#Ops[X]
}
