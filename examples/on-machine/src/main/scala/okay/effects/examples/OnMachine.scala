package okay.effects.examples

import okay.effects.simple.Machine

/** the examples ON THE MACHINE: `sbt examplesOnMachine/run` */
object OnMachine {
  def main(args: Array[String]): Unit = Report.lines(Machine).foreach(println)
}
