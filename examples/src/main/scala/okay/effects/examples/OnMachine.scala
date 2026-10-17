package okay.effects.examples

import okay.effects.machine.Machine

/** the examples ON THE MACHINE: `sbt "examples/runMain okay.effects.examples.OnMachine"` */
object OnMachine {
  implicit val fx: okay.effects.Effects = Machine
  def main(args: Array[String]): Unit = Report.lines.foreach(println)
}
