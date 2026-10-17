package okay.effects.examples

import okay.effects.simple.free.Free

/** the examples ON THE `Free` MONAD: `sbt examplesOnFree/run` */
object OnFree {
  def main(args: Array[String]): Unit = Report.lines(Free).foreach(println)
}
