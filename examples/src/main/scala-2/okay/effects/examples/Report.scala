package okay.effects.examples

import okay.effects.Effects

/** the examples run on `fx`, each value with its name: what a runner prints */
object Report {
  def lines(fx: Effects): List[String] = {
    val b = new Basics2[fx.type](fx)
    List(
      s"greeted       ${b.greeted}",
      s"counted       ${b.counted}",
      s"firstEven     ${b.firstEven(List(1, 3, 4, 6))}",
      s"served        ${new Shared(fx).served(List("Ann", "Bob"))}")
  }
}
