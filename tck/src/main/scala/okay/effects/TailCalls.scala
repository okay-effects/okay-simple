package okay.effects

/** mutual tail recursion and loops: one source for both compilers, rows by aliases */
class TailCalls(val fx: Runner) {
  import fx._
  import fx.std._

  type St[X] = State[Int, X]

  /** two functions calling each other in tail position, a million times, with no effect */
  def even(n: Int): Boolean ! Pure = if (n == 0) pure[Boolean, Pure](true) else tailcall(odd(n - 1))
  def odd(n: Int): Boolean ! Pure = if (n == 0) pure[Boolean, Pure](false) else tailcall(even(n - 1))

  /** the same, counting each call in a state */
  def evenCounted(n: Int): Boolean ! St =
    if (n == 0) pure[Boolean, St](true) else modify[Int, St](_ + 1).flatMap(_ => tailcall(oddCounted(n - 1)))
  def oddCounted(n: Int): Boolean ! St =
    if (n == 0) pure[Boolean, St](false) else modify[Int, St](_ + 1).flatMap(_ => tailcall(evenCounted(n - 1)))

  def isEven(n: Int): Boolean = run(even(n))
  def isOdd(n: Int): Boolean = run(odd(n))
  def counted(n: Int): (Int, Boolean) = run(state(0)(evenCounted(n)))

  /** a loop: the sum of 1 to n, its steps in a state */
  def summed(n: Int): (Int, Long) = run(state(0)(tailRecM[(Int, Long), Long, St]((n, 0L)) { case (i, acc) =>
    modify[Int, St](_ + 1).as(if (i == 0) Right(acc) else Left((i - 1, acc + i)))
  }))
}
