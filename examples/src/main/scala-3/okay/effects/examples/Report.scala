package okay.effects.examples

import okay.effects.Effects

/** the examples run on `fx`, each value with its name: what a runner prints */
object Report:
  def lines(fx: Effects): List[String] =
    val b = Basics3(fx)
    val c = Control3(fx)
    List(
      s"greeted       ${b.greeted}",
      s"counted       ${b.counted}",
      s"twoFlips      ${b.twoFlips}",
      s"collected     ${c.collected}",
      s"firstTwo      ${c.firstTwo}",
      s"summed        ${c.summed}",
      s"pythagorean   ${c.pythagorean(20)}",
      s"halves        ${c.halves}",
      s"twice         ${c.twice}",
      s"greeting      ${c.greeting}",
      s"served        ${Shared(fx).served(List("Ann", "Bob"))}")
