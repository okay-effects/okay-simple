package okay.effects.examples

import okay.effects.simple.free.Free

/** the examples ON THE `Free` MONAD: `sbt "examples/runMain okay.effects.examples.OnFree"` */
object OnFree {
  def main(args: Array[String]): Unit = Report.lines(Free).foreach(println)
}
