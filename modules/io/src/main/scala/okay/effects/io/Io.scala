package okay.effects.io

import okay.effects.Effects

/** a console's operations: a line read, a line written */
sealed trait Console[X]
object Console {
  final case class ReadLine[X](answer: Option[String] => X) extends Console[X]
  final case class PrintLine[X](s: String, answer: Unit => X) extends Console[X]
}
/** a clock's: the time now, in milliseconds */
sealed trait Clock[X]
object Clock {
  final case class Now[X](answer: Long => X) extends Clock[X]
}
/** the environment's: a variable by name */
sealed trait Env[X]
object Env {
  final case class Get[X](name: String, answer: Option[String] => X) extends Env[X]
}

/**
 * CAPABILITIES over any implementation `fx`: the console, the clock, the environment — each an effect, so a program
 * names what it touches, and its handler says how: the WORLD's (`realConsole`, `systemClock`, `systemEnv`: the
 * operation done where it is answered, in place) or a PURE one for tests (`console(inputs)`, `clock(start, step)`,
 * `env(vars)`). The same program under either.
 */
class Io[F <: Effects](val fx: F) {
  import fx._

  def readLine[R[_]](implicit in: In[Console, R]): Option[String] ! R = perform[Console].apply[R, Option[String]](Console.ReadLine(l => l))
  def printLine[R[_]](s: String)(implicit in: In[Console, R]): Unit ! R = perform[Console].apply[R, Unit](Console.PrintLine(s, u => u))
  def now[R[_]](implicit in: In[Clock, R]): Long ! R = perform[Clock].apply[R, Long](Clock.Now(t => t))
  def getEnv[R[_]](name: String)(implicit in: In[Env, R]): Option[String] ! R = perform[Env].apply[R, Option[String]](Env.Get(name, v => v))

  /** the lines read from `inputs` in turn (`None` past them), and the lines written, in order, with the value */
  def console(inputs: List[String]): ConsoleHandler = new ConsoleHandler(inputs)
  final class ConsoleHandler(inputs: List[String]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Console, R, Rest]): (List[String], A) ! Rest =
      handle[Console, (List[String], List[String]), R, Rest, A, (List[String], A)](p, (inputs, Nil),
        new StatefulTailHandler[Console, (List[String], List[String]), Rest, A, (List[String], A)] {
          def ret(s: (List[String], List[String]), a: A) = pure((s._2.reverse, a))
          def answer[X](cell: Cell[(List[String], List[String])], op: Console[X]): X = op match {
            case Console.ReadLine(answer) => cell.get._1 match {
              case l :: ls => cell.set((ls, cell.get._2)); answer(Some(l))
              case Nil => answer(None)
            }
            case Console.PrintLine(s, answer) => cell.set((cell.get._1, s :: cell.get._2)); answer(())
          }
        })
  }
  /** the process's console: standard input and output */
  def realConsole[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Console, R, Rest]): A ! Rest =
    handle[Console, R, Rest, A, A](p, new TailHandler[Console, Rest, A, A] {
      def ret(a: A) = pure(a)
      def answer[X](op: Console[X]): X = op match {
        case Console.ReadLine(answer) => answer(Option(scala.io.StdIn.readLine()))
        case Console.PrintLine(s, answer) => println(s); answer(())
      }
    })

  /** a clock from `start`, `step` milliseconds on at each reading */
  def clock(start: Long, step: Long): ClockHandler = new ClockHandler(start, step)
  final class ClockHandler(start: Long, step: Long) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Clock, R, Rest]): A ! Rest =
      handle[Clock, Long, R, Rest, A, A](p, start, new StatefulTailHandler[Clock, Long, Rest, A, A] {
        def ret(t: Long, a: A) = pure(a)
        def answer[X](cell: Cell[Long], op: Clock[X]): X = op match {
          case Clock.Now(answer) => val t = cell.get; cell.set(t + step); answer(t)
        }
      })
  }
  def systemClock[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Clock, R, Rest]): A ! Rest =
    handle[Clock, R, Rest, A, A](p, new TailHandler[Clock, Rest, A, A] {
      def ret(a: A) = pure(a)
      def answer[X](op: Clock[X]): X = op match { case Clock.Now(answer) => answer(System.currentTimeMillis()) }
    })

  /** the environment `vars` */
  def env(vars: Map[String, String]): EnvHandler = new EnvHandler(vars)
  final class EnvHandler(vars: Map[String, String]) {
    def apply[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Env, R, Rest]): A ! Rest =
      handle[Env, R, Rest, A, A](p, new TailHandler[Env, Rest, A, A] {
        def ret(a: A) = pure(a)
        def answer[X](op: Env[X]): X = op match { case Env.Get(name, answer) => answer(vars.get(name)) }
      })
  }
  def systemEnv[R[_], Rest[_], A](p: A ! R)(implicit sp: Split[Env, R, Rest]): A ! Rest =
    handle[Env, R, Rest, A, A](p, new TailHandler[Env, Rest, A, A] {
      def ret(a: A) = pure(a)
      def answer[X](op: Env[X]): X = op match { case Env.Get(name, answer) => answer(sys.env.get(name)) }
    })
}

object Io {
  def apply(fx: Effects): Io[fx.type] = new Io[fx.type](fx)
}
