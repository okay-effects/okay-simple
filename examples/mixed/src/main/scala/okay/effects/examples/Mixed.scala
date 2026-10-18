package okay.effects.examples

import okay.effects.Runner

/**
 * ONE APPLICATION OF TWO COMPILERS, over any implementation `fx`, given: Scala 3 code taking the effect, the handler and the
 * program Scala 2.13 compiled (`Basics2`, `Shared`) as they are, beside its own, in one row and one run. An
 * application with Scala 2 code in it takes the interface's Scala 2.13 build, which Scala 3 reads: so its Scala 3
 * code writes rows as Scala 2 does, by names, `#L`, and the context bounds `Has[E]#L`.
 */
class Mixed(using val fx: Runner):
  import fx.*
  import fx.std.*
  /** Scala 2's code over the same `fx`: its types this code's */
  val basics = Basics2[fx.type]()(using fx)

  /** a Scala 3 program over Scala 2's effect and a standard one */
  def doubled[R[_]: Has[Basics2.Counter]#L: Has[(State % Int)#L]#L]: Int ! R =
    basics.tick[R].andThen(basics.peek[R]).flatMap(n => modify[Int, R](_ + 2 * n).as(n))

  type Row[X] = (*[Basics2.Counter] + (State % Int))#L[X]

  /** Scala 2's program and handler with Scala 3's program, one row, one run */
  def both: (Int, (Int, Int)) = run(state(0)(basics.counting(basics.tick[Row].andThen(doubled[Row]))))

  /** the shared code's program, written once, run from here */
  def served: (List[String], Unit) = Shared().served(List("Ann"))
