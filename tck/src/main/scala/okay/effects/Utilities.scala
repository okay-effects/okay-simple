package okay.effects

/** the standard effects' smaller words: `local`, `listen`, `censor`, `gets`, `getAndSet`, `guard`, `abort`, `fresh`,
 * `random`, `store` — one source for both compilers, rows by aliases */
class Utilities(val fx: Runner) {
  import fx._
  import fx.std._

  type Rd[X] = Reader[Int, X]
  type Wr[X] = Writer[String, X]
  type St[X] = State[Int, X]
  type RW[X] = Sum[({ type L[x] = Sum[Void, Rd, x] })#L, Wr, X]

  /** the environment seen inside `local`, then outside again */
  def locally: (Int, Int, Int) = run(reader(10)(
    ask[Int, Rd].flatMap(a => local[Int](_ * 2)(ask[Int, Rd]).flatMap(b => ask[Int, Rd].map(c => (a, b, c))))))
  /** `local` deep in a row of two */
  def localInRow: (List[String], Int) = run(reader(1)(writer[String](
    local[Int](_ + 41)(ask[Int, RW].flatMap(e => tell[String, RW](s"e=$e").as(e))))))

  def listened: (List[String], (List[String], Int)) = run(writer[String](
    tell[String, Wr]("a").flatMap(_ => listen[String](tell[String, Wr]("b").flatMap(_ => tell[String, Wr]("c")).as(3)))))
  def censored: (List[String], Int) = run(writer[String](
    tell[String, Wr]("a").flatMap(_ => censor[String](_.map(_.toUpperCase))(tell[String, Wr]("b").as(1)))))

  def stateWords: (Int, (Int, Int)) = run(state(5)(gets[Int, Int, St](_ * 10).flatMap(g => getAndSet[Int, St](7).map(o => (g, o)))))

  def guarded: List[Int] = run(all(among[Int, Choose]((1 to 10).toList).flatMap(x => guard[Choose](x % 3 == 0).as(x))))

  type Ab[X] = Sum[({ type L[x] = Sum[Void, Abort, x] })#L, St, X]
  /** aborted: the state's handler outside sees what was put before */
  def aborted: (Int, Option[Int]) = run(state(0)(optional(put[Int, Ab](1).flatMap(_ => abort[Int, Ab]).map(_ + 1))))
  def notAborted: Option[Int] = run(optional(pure[Int, Abort](3)))

  def freshOnes: List[Long] = run(freshFrom(100)(fresh[Fresh].flatMap(a => fresh[Fresh].flatMap(b => fresh[Fresh].map(c => List(a, b, c))))))
  /** fresh in the branches of a choice: the supply inside the choice, each branch's from its time; outside, one
   * supply through every branch */
  type FC[X] = Sum[({ type L[x] = Sum[Void, Choose, x] })#L, Fresh, X]
  def twoWays: (Int, Long) ! FC = fresh[FC].flatMap(_ => among[Int, FC](List(1, 2))).flatMap(x => fresh[FC].map(n => (x, n)))
  def freshEachBranch: List[(Int, Long)] = run(all(freshFrom(0)(twoWays)))
  def freshThroughBranches: List[(Int, Long)] = run(freshFrom(0)(all(twoWays)))

  def randoms(seed: Long): List[Int] =
    run(random(seed)((1 to 5).toList.foldLeft(pure[List[Int], Random](Nil))((p, _) => p.flatMap(l => nextInt[Random](100).map(_ :: l)))))
  def doubleInRange: Boolean = run(random(1)(nextDouble[Random].map(d => d >= 0 && d < 1)))

  type KV[X] = Store[String, Int, X]
  def stored: (Map[String, Int], (Option[Int], Option[Int])) = run(store[String, Int](Map("a" -> 1))(
    save[String, Int, KV]("b", 2).flatMap(_ => remove[String, Int, KV]("a")).flatMap(_ =>
      load[String, Int, KV]("a").flatMap(a => load[String, Int, KV]("b").map(b => (a, b))))))
}
