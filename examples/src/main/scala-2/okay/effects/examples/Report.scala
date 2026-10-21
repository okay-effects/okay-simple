package okay.effects.examples

import okay.effects.Runner

/** the examples run on the implementation given, each value with its name: what a runner prints */
object Report {
  def lines(implicit fx: Runner): List[String] = {
    val b = new Basics2[fx.type]()(fx)   // the implementation given at the call: its singleton type is the examples'
    List(
      s"greeted       ${b.greeted}",
      s"counted       ${b.counted}",
      s"firstEven     ${b.firstEven(List(1, 3, 4, 6))}",
      s"served        ${new Shared().served(List("Ann", "Bob"))}")
  }
}
