package okay.effects.async

import java.util.concurrent.{Executors, ScheduledExecutorService, ThreadFactory, TimeUnit}

/** the JVM's timer: one daemon thread, its callbacks short — they resume a program on its execution context */
private[async] object Timers {
  private lazy val scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    def newThread(r: Runnable): Thread = { val t = new Thread(r, "okay-effects-timer"); t.setDaemon(true); t }
  })
  def after(ms: Long)(f: () => Unit): Unit = { scheduler.schedule(new Runnable { def run(): Unit = f() }, ms, TimeUnit.MILLISECONDS); () }
}
