package okay.effects.async

import scala.concurrent.{ExecutionContext, Future, Promise}
import java.util.concurrent.atomic.AtomicReference
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import okay.effects.Runner

/** AN ASYNCHRONOUS OPERATION: `Suspend(register)`, the program waiting until `register`'s callback is called — with
 * its value, or its failure. In a row, `Async` */
sealed trait Async[X]
object Async {
  final case class Suspend[X](register: (Either[Throwable, X] => Unit) => Unit) extends Async[X]
}

/** where an asynchronous program stands: done, or waiting — `k` the rest, a program `M[...]` of no effect */
sealed abstract class Waiting[A, M[_]]
object Waiting {
  final case class Done[A, M[_]](a: A) extends Waiting[A, M]
  final case class Suspended[A, M[_], X](register: (Either[Throwable, X] => Unit) => Unit, k: X => M[Waiting[A, M]])
      extends Waiting[A, M]
}

/**
 * ASYNCHRONY over any implementation that runs, `fx`, with no runtime of its own: a program of `Async` waits on
 * callbacks; `toFuture(p)` drives it — runs it to its next wait, registers the callback, and when the callback is
 * called resumes the rest, on `ec`, until it is done: a `Future` of its value, failed if a callback gave a failure or
 * the program threw. The other effects of a program are handled before, its row then `Async` alone. On the JVM and on
 * Scala.js; `sleep` by the platform's timer.
 *
 * On it: `fromFuture`, `sleep`, `both` and `parTraverse` (run at once, each its own `toFuture`), `race`, `timeout`.
 */
class Asynchrony[F <: Runner](val fx: F)(implicit ec: ExecutionContext) {
  import fx._

  /** an operation answered by a callback, called once */
  def async[X, R[_]](register: (Either[Throwable, X] => Unit) => Unit)(implicit in: In[Async, R]): X ! R =
    perform[Async].apply[R, X](Async.Suspend(register))
  /** `f`'s value when it completes; `f` made when the program gets here */
  def fromFuture[X, R[_]](f: => Future[X])(implicit in: In[Async, R]): X ! R =
    async[X, R](cb => f.onComplete { case Success(x) => cb(Right(x)); case Failure(e) => cb(Left(e)) })
  /** `ms` milliseconds later */
  def sleep[R[_]](ms: Long)(implicit in: In[Async, R]): Unit ! R = async[Unit, R](cb => Timers.after(ms)(() => cb(Right(()))))

  private type M[x] = x ! Pure
  private def coroutine[A](p: A ! Async): Waiting[A, M] ! Pure =
    handle[Async, Async, Pure, A, Waiting[A, M]](p, new Handler[Async, Pure, A, Waiting[A, M]] {
      def ret(a: A) = pure(Waiting.Done[A, M](a))
      def op[X](op: Async[X], k: X => Waiting[A, M] ! Pure) = op match {
        case Async.Suspend(register) => pure(Waiting.Suspended[A, M, X](register, k))
      }
    })

  /** the program driven to its end: its value, a `Future` */
  def toFuture[A](p: A ! Async): Future[A] = {
    val done = Promise[A]()
    drive(coroutine(p), done)
    done.future
  }
  /** a loop: a callback called while `register` runs — a future already done — is taken here and the loop goes on;
   * one called later resumes the rest on `ec`. Which of the two, one `compareAndSet` decides */
  private def drive[A](p: Waiting[A, M] ! Pure, done: Promise[A]): Unit = {
    var next: Option[Waiting[A, M] ! Pure] = Some(p)
    try while (next.isDefined) {
      val now = next.get
      next = None
      fx.run(now) match {
        case Waiting.Done(a) => done.success(a)
        case s: Waiting.Suspended[A, M, x] @unchecked =>
          val slot = new AtomicReference[Either[Throwable, x]](null)
          s.register { r =>
            if (!slot.compareAndSet(null, r)) r match {
              case Right(x) => ec.execute(() => drive(s.k(x), done))
              case Left(e) => done.tryFailure(e)
            }
          }
          if (!slot.compareAndSet(null, later)) slot.get match {
            case Right(x) => next = Some(s.k(x))
            case Left(e) => done.tryFailure(e)
          }
      }
    } catch { case NonFatal(e) => done.tryFailure(e) }
  }
  /** the slot's mark: `register` returned before its callback was called */
  private object Later extends RuntimeException("later", null, false, false)
  private val later: Either[Throwable, Nothing] = Left(Later)

  /** both at once, each driven on its own: both values */
  def both[A, B](p: A ! Async, q: B ! Async): (A, B) ! Async = {
    def started = { val fa = toFuture(p); val fb = toFuture(q); fa.zip(fb) }
    fromFuture[(A, B), Async](started)
  }
  /** each element's program at once: their values, in order */
  def parTraverse[X, A](xs: List[X])(f: X => A ! Async): List[A] ! Async =
    fromFuture[List[A], Async](Future.sequence(xs.map(x => toFuture(f(x)))))
  /** the first to end: its value, the other's left to finish unseen */
  def race[A, B](p: A ! Async, q: B ! Async): Either[A, B] ! Async =
    fromFuture[Either[A, B], Async](Future.firstCompletedOf(List(toFuture(p).map(Left(_)), toFuture(q).map(Right(_)))))
  /** the value if the program ends within `ms` milliseconds */
  def timeout[A](ms: Long)(p: A ! Async): Option[A] ! Async =
    race(p, sleep[Async](ms)).map(_.left.toOption)
}

object Asynchrony {
  def apply(fx: Runner)(implicit ec: ExecutionContext): Asynchrony[fx.type] = new Asynchrony[fx.type](fx)
}
