package okay.effects.async

import scala.concurrent.{ExecutionContext, Future, Promise}
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import okay.effects.Runner

/** AN ASYNCHRONOUS OPERATION: `Suspend(register)`, the program waiting until `register`'s callback is called — with
 * its value, or its failure; `register` gives back how to cancel the wait (a timer cleared, a fiber interrupted). In a
 * row, `Async` */
sealed trait Async[X]
object Async {
  final case class Suspend[X](register: (Either[Throwable, X] => Unit) => (() => Unit)) extends Async[X]
}

/** where an asynchronous program stands: done, or waiting — `k` the rest, a program `M[...]` of no effect */
sealed abstract class Waiting[A, M[_]]
object Waiting {
  final case class Done[A, M[_]](a: A) extends Waiting[A, M]
  final case class Suspended[A, M[_], X](register: (Either[Throwable, X] => Unit) => (() => Unit),
    k: X => M[Waiting[A, M]])
    extends Waiting[A, M]
}

/** a program running: its value to come, and how to cancel it — the wait it is in cancelled, the rest never run, the
 * future failed with `Cancelled` */
final class Running[A](val future: Future[A], cancelling: () => Unit) {
  def cancel(): Unit = cancelling()
}
/** what a cancelled program's future fails with */
final class Cancelled extends RuntimeException("cancelled")

/**
 * ASYNCHRONY over any implementation that runs, `fx`, with no runtime of its own: a program of `Async` waits on
 * callbacks; `toFuture(p)` drives it — runs it to its next wait, registers the callback, and when the callback is
 * called resumes the rest, on `ec`, until it is done: a `Future` of its value, failed if a callback gave a failure or
 * the program threw. The other effects of a program are handled before, its row then `Async` alone. On the JVM and on
 * Scala.js; `sleep` by the platform's timer.
 *
 * On it: `fromFuture`, `sleep`, `both` and `parTraverse` (run at once, each its own run), `race`, `timeout`. CANCELLED:
 * `start(p)` gives a `Running`, its `cancel()` the wait it is in cancelled and the rest never run — a timer cleared,
 * a fiber interrupted; `race` and `timeout` cancel the program that lost, `both` and `parTraverse` the others when one
 * fails.
 */
class Asynchrony[F <: Runner with Singleton](implicit implementation: F, ec: ExecutionContext) {
  /** the implementation — a value, not an implicit: a package of this module's on the machine gives no implementation
   * to the code it is imported into */
  val fx: F = implementation
  import fx._

  /** an operation answered by a callback, called once; it cannot be cancelled */
  def async[X, R[_]](register: (Either[Throwable, X] => Unit) => Unit)(implicit in: In[Async, R]): X ! R =
    cancellable[X, R] { cb => register(cb); Asynchrony.noCancel }
  /** an operation answered by a callback, called once; `register` gives back how to cancel it */
  def cancellable[X, R[_]](register: (Either[Throwable, X] => Unit) => (() => Unit))(implicit in: In[Async, R]): X ! R =
    perform[Async].apply[R, X](Async.Suspend(register))
  /** `f`'s value when it completes; `f` made when the program gets here */
  def fromFuture[X, R[_]](f: => Future[X])(implicit in: In[Async, R]): X ! R =
    async[X, R](cb => f.onComplete { case Success(x) => cb(Right(x)); case Failure(e) => cb(Left(e)) })
  /** `ms` milliseconds later */
  def sleep[R[_]](ms: Long)(implicit in: In[Async, R]): Unit ! R =
    cancellable[Unit, R](cb => Timers.after(ms)(() => cb(Right(()))))

  private type M[x] = x ! Pure
  private def coroutine[A](p: A ! Async): Waiting[A, M] ! Pure =
    handle[Async, Async, Pure, A, Waiting[A, M]](p,
      new Handler[Async, Pure, A, Waiting[A, M]] {
        def ret(a: A) = pure(Waiting.Done[A, M](a))
        def op[X](op: Async[X], k: X => Waiting[A, M] ! Pure) = op match {
          case Async.Suspend(register) => pure(Waiting.Suspended[A, M, X](register, k))
        }
      })

  /** the program driven to its end: its value, a `Future` */
  def toFuture[A](p: A ! Async): Future[A] = start(p).future

  /** the program started: its future, and how to cancel it */
  def start[A](p: A ! Async): Running[A] = {
    val done = Promise[A]()
    val run = new Run
    drive(coroutine(p), done, run)
    new Running[A](done.future, () => run.cancel(done))
  }

  /** a run's state: cancelled or not, and how to cancel the wait it is in */
  private final class Run {
    @volatile var cancelled = false
    val waiting = new AtomicReference[() => Unit](Asynchrony.noCancel)
    def cancel[B](done: Promise[B]): Unit = if (!cancelled) {
      cancelled = true
      waiting.getAndSet(Asynchrony.noCancel)()
      done.tryFailure(new Cancelled)
      ()
    }
    /** the wait now: cancelled at once if the run was, meanwhile */
    def waitingOn(c: () => Unit): Unit = { waiting.set(c); if (cancelled) waiting.getAndSet(Asynchrony.noCancel)() }
  }

  /** a loop: a callback called while `register` runs — a future already done — is taken here and the loop goes on;
   * one called later resumes the rest on `ec`. Which of the two, one `compareAndSet` decides; a run cancelled goes
   * no further */
  private def drive[A](p: Waiting[A, M] ! Pure, done: Promise[A], run: Run): Unit = {
    var next: Option[Waiting[A, M] ! Pure] = Some(p)
    try while (next.isDefined && !run.cancelled) {
        val now = next.get
        next = None
        fx.run(now) match {
          case Waiting.Done(a) => done.trySuccess(a)
          case s: Waiting.Suspended[A, M, x] @unchecked =>
            val slot = new AtomicReference[Either[Throwable, x]](null)
            val cancel = s.register { r =>
              if (!slot.compareAndSet(null, r) && !run.cancelled) {
                run.waiting.set(Asynchrony.noCancel)
                r match {
                  case Right(x) => ec.execute(() => drive(s.k(x), done, run))
                  case Left(e) => done.tryFailure(e)
                }
              }
            }
            if (slot.compareAndSet(null, later)) run.waitingOn(cancel)
            else slot.get match {
              case Right(x) => next = Some(s.k(x))
              case Left(e) => done.tryFailure(e)
            }
        }
      }
    catch { case NonFatal(e) => done.tryFailure(e) }
  }
  /** the slot's mark: `register` returned before its callback was called */
  private object Later extends RuntimeException("later", null, false, false)
  private val later: Either[Throwable, Nothing] = Left(Later)

  /** both at once, each run on its own: both values; one failing, the other cancelled; this cancelled, both */
  def both[A, B](p: A ! Async, q: B ! Async): (A, B) ! Async = cancellable[(A, B), Async] { cb =>
    val ra = start(p)
    val rb = start(q)
    ra.future.failed.foreach(_ => rb.cancel())
    rb.future.failed.foreach(_ => ra.cancel())
    ra.future.zip(rb.future).onComplete { case Success(ab) => cb(Right(ab)); case Failure(e) => cb(Left(e)) }
    () => { ra.cancel(); rb.cancel() }
  }
  /** each element's program at once: their values, in order; one failing, the others cancelled */
  def parTraverse[X, A](xs: List[X])(f: X => A ! Async): List[A] ! Async = cancellable[List[A], Async] { cb =>
    val runs = xs.map(x => start(f(x)))
    runs.foreach(r => r.future.failed.foreach(_ => runs.foreach(_.cancel())))
    Future.sequence(runs.map(_.future)).onComplete { case Success(as) => cb(Right(as)); case Failure(e) => cb(Left(e)) }
    () => runs.foreach(_.cancel())
  }
  /** the first to end: its value — the other cancelled */
  def race[A, B](p: A ! Async, q: B ! Async): Either[A, B] ! Async = cancellable[Either[A, B], Async] { cb =>
    val ra = start(p)
    val rb = start(q)
    Future.firstCompletedOf(List(ra.future.map(Left(_)), rb.future.map(Right(_)))).onComplete { r =>
      ra.cancel(); rb.cancel()
      r match { case Success(v) => cb(Right(v)); case Failure(e) => cb(Left(e)) }
    }
    () => { ra.cancel(); rb.cancel() }
  }
  /** the value if the program ends within `ms` milliseconds: else `None`, the program cancelled */
  def timeout[A](ms: Long)(p: A ! Async): Option[A] ! Async =
    race(p, sleep[Async](ms)).map(_.left.toOption)
}

object Asynchrony {
  /** the platform's default execution context: the global one on the JVM and Scala Native, the microtask queue on
   * Scala.js */
  def defaultContext: ExecutionContext = Timers.executionContext
  /** a wait that cannot be cancelled */
  val noCancel: () => Unit = () => ()

  def apply()(implicit fx: Runner, ec: ExecutionContext): Asynchrony[fx.type] = {
    implicit val exact: fx.type = fx
    new Asynchrony[fx.type]
  }
}
