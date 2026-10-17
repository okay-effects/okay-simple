package okay.effects.examples

import okay.effects.free.Free

/** the examples ON THE `Free` MONAD: `sbt "examples/runMain okay.effects.examples.OnFree"` */
object OnFree {
  implicit val fx: okay.effects.Effects = Free
  def main(args: Array[String]): Unit = Report.lines.foreach(println)
}
