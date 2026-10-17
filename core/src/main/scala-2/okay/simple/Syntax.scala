package okay.simple

/** Scala 2's conveniences: none beyond the interface's own — everything Scala 2 code needs is in the source both
 * compilers build (`Has[E]#L`, `HandlerOf`), so that it is there in Scala 3's build too */
trait Syntax { self: Effects => }
