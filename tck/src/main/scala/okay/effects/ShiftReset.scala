package okay.effects

/** shift and reset, one source for both compilers: a row of one effect, `Control`, by aliases */
class ShiftReset(val fx: Runner) {
  import fx._
  import fx.std._

  type C[X] = Control[Int ! Pure, X]
  type Row[X] = Sum[Void, C, X]

  /** `k` resumed twice: 1 + (1 + 10) */
  def twice: Int = run(reset[Int, Pure](shift[Int, Int, Pure, Row](k => k(10).flatMap(k)).map(_ + 1)))

  /** `k` dropped: the reset's value `f`'s */
  def dropped: Int = run(reset[Int, Pure](shift[Int, Int, Pure, Row](_ => pure(42)).map(_ + 1)))

  type CL[X] = Control[List[Int] ! Pure, X]
  type RowL[X] = Sum[Void, CL, X]
  /** each resumption its own: the rest once for 1, once for 2 */
  def both: List[Int] = run(reset[List[Int], Pure](shift[Int, List[Int], Pure, RowL](k =>
        k(1).flatMap(a => k(2).map(b => a ++ b))).map(x => List(x * 10))))
}
