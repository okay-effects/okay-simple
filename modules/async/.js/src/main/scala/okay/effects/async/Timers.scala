package okay.effects.async

/** Scala.js's timer: the host's `setTimeout` */
private[async] object Timers {
  /** the default's execution context: the host's queue of microtasks, as Scala.js's global one is, named */
  def executionContext: scala.concurrent.ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
  def after(ms: Long)(f: () => Unit): Unit = { scala.scalajs.js.timers.setTimeout(ms.toDouble)(f()); () }
}
