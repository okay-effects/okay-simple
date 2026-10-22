package okay.effects.async

import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory, TimeUnit}

/** Scala Native's timer, as the JVM's: one daemon thread, its callbacks short — they resume a program on its execution
 * context */
private[async] object Timers {
  private lazy val scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(r: Runnable): Thread = { val t = new Thread(r, "okay-effects-timer"); t.setDaemon(true); t }
  })
  /** the default's execution context: the global one */
  def executionContext: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  /** `f` after `ms` milliseconds; what is given back cancels it */
  def after(ms: Long)(f: () => Unit): () => Unit = {
    val scheduled = scheduler.schedule(new Runnable { def run(): Unit = f() }, ms, TimeUnit.MILLISECONDS)
    () => { scheduled.cancel(false); () }
  }
}
