package okay.effects.async

/** Scala.js's timer: the host's `setTimeout` */
private[async] object Timers {
  def after(ms: Long)(f: () => Unit): Unit = { scala.scalajs.js.timers.setTimeout(ms.toDouble)(f()); () }
}
