package okay.effects.examples

import okay.effects.Runner

/** the examples run on the implementation given, each value with its name: what a runner prints */
object Report:
  def lines(using fx: Runner): List[String] =
    val b = Basics3()
    val c = Control3()
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
      s"served        ${Shared().served(List("Ann", "Bob"))}")
