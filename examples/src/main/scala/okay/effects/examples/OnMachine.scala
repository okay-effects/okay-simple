package okay.effects.examples

import okay.effects.simple.Machine

/** the examples ON THE MACHINE: `sbt "examples/runMain okay.effects.examples.OnMachine"` */
object OnMachine {
  def main(args: Array[String]): Unit = Report.lines(Machine).foreach(println)
}
