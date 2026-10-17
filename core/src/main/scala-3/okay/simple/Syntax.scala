package okay.simple

/** Scala 3's conveniences, over the same types. With Scala 3's precedence a row needs no parentheses,
 * `Int ! Reader % String + State % Int`; the operations of `F % S` for a handler, `Handler[State Of Int, R, A, B]`.
 * Scala 2 code needs none of these: a Scala 2 module reading Scala 3's build uses the interface's own forms */
trait Syntax { self: Effects =>
  infix type Of[F[_, _], S] = [x] =>> F[S, x]
}
