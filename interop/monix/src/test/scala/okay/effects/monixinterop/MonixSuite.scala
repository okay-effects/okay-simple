package okay.effects.monixinterop

import _root_.monix.eval.Task
import _root_.monix.execution.Scheduler
import _root_.monix.reactive.Observable
import okay.effects.Runner
import okay.effects.machine.Machine
import okay.effects.free.Free

abstract class MonixChecks(val fx0: Runner) extends munit.FunSuite {
  private implicit val scheduler: Scheduler = Scheduler.global
  implicit val runner: Runner = fx0
  val m: Monix[runner.type] = Monix()
  import m._
  import m.fx._
  import m.streams._

  test("a task in a program, a program a task") {
    val p = fromTask[Int, Tasks](Task(20)).flatMap(a => fromTask[Int, Tasks](Task.eval(a + 22)))
    toTask(p).runToFuture.map(v => assertEquals(v, 42))
  }
  test("a stream of tasks as an Observable; one of no effect, endless, taken on monix's side") {
    val s = range[Tasks](1, 4).evalMap(n => fromTask[Int, Tasks](Task(n * 10)))
    toObservable(s).toListL.runToFuture.map(xs => assertEquals(xs, List(10, 20, 30)))
      .flatMap(_ => toObservablePure(iterate[Int, Pure](1)(_ * 2)).take(4).toListL.runToFuture)
      .map(xs => assertEquals(xs, List(1, 2, 4, 8)))
  }
  test("an Observable as a stream: pulled as far as asked, transformed here") {
    val s = fromObservable(Observable.range(0, 1000000).map(_.toInt * 2)).filter(_ % 3 == 0).take(4)
    toTask(s.toList).runToFuture.map(xs => assertEquals(xs, List(0, 6, 12, 18)))
  }
  test("an Observable read to its end; its failure the task's") {
    toTask(fromObservable(Observable(1, 2, 3)).fold(0)(_ + _)).runToFuture.map(n => assertEquals(n, 6))
      .flatMap(_ => toTask(fromObservable(Observable(1) ++ Observable.raiseError(new RuntimeException("boom"))).toList)
        .attempt.runToFuture)
      .map(e => assertEquals(e.left.map(_.getMessage), Left("boom")))
  }
}

class MonixOnMachine extends MonixChecks(Machine)
class MonixOnFree extends MonixChecks(Free)
class MonixOnTagless extends MonixChecks(okay.effects.tagless.Tagless.on(Machine))
