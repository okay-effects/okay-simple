package okay.effects.algebra.checks

import okay.effects.Runner
import okay.effects.algebra.{Algebra, Decide, History, Kept}
import okay.effects.free.Free
import okay.effects.machine.Machine
import okay.effects.TailClauses
import scala.util.Random

sealed trait Cmd[X]
final case class Exec(cmd: String) extends Cmd[Boolean]

/** A COEFFECT OVER AN EFFECT, on an implementation: commands refused after three failed in a row (the decision reads
 * the history behind each); the audit by `extend` over the history kept gives the decisions made live */
abstract class OverHistoryLaws(val fx0: Runner, name: String) extends munit.FunSuite {
  val alg: Algebra[fx0.type] = new Algebra[fx0.type]()(fx0)
  import fx0._

  /** a command as kept: its text, and whether it went well (none while pending) */
  type Seen = (String, Option[Boolean])
  val kept: Kept[Cmd, Seen] = new Kept[Cmd, Seen] {
    def pending[X](op: Cmd[X]): Seen = op match { case Exec(c) => (c, None) }
    def ended[X](op: Cmd[X], answer: X): Seen = op match { case Exec(c) => (c, Some(answer)) }
  }
  /** refused: the third failure in a row behind it */
  def refuse(h: History[Seen]): Boolean = h.past.reverseIterator.takeWhile(_._2.contains(false)).size >= 3

  def session(cmds: Vector[String], decisions: scala.collection.mutable.Buffer[Boolean]): (Vector[Boolean], Int) = {
    var ran = 0
    val decide: Decide[Cmd, Seen] = new Decide[Cmd, Seen] {
      def apply[X](h: History[Seen], op: Cmd[X]): Option[X] = op match {
        case Exec(_) => val no = refuse(h); decisions += no; if (no) Some(false) else None
      }
    }
    val program: Vector[Boolean] ! Cmd =
      alg.traverse[String, Boolean, Cmd](cmds)(c => perform[Cmd].apply[Cmd, Boolean](Exec(c)))
    val handled = handle[Cmd, Cmd, Pure, Vector[Boolean], Vector[Boolean]](
      alg.overHistory[Cmd, Seen, Cmd, Vector[Boolean]](program)(kept, decide),
      new TailClauses[Cmd, Vector[Boolean], Vector[Boolean] ! Pure] {
        def ret(a: Vector[Boolean]) = pure[Vector[Boolean], Pure](a)
        def answer[X](op: Cmd[X]): X = op match { case Exec(c) => ran += 1; !c.startsWith("bad") }
      })
    (run(handled), ran)
  }

  test(s"$name: refused after three failures in a row, the world not asked; a success resets it") {
    val d = scala.collection.mutable.Buffer.empty[Boolean]
    val (answers, ran) = session(Vector("bad1", "bad2", "bad3", "ls", "ls"), d)
    assertEquals(answers, Vector(false, false, false, false, false))
    assertEquals(ran, 3)
    assertEquals(d.toList, List(false, false, false, true, true))
    assertEquals(session(Vector("bad1", "bad2", "ok", "bad3", "ls"), d)._2, 5)
  }

  test(s"$name: the audit's law — extend of the decision over the history kept gives the decisions made live") {
    var refused = 0
    for (seed <- 1 to 300) {
      val r = new Random(seed)
      val cmds = Vector.fill(1 + r.nextInt(15))(if (r.nextDouble() < 0.5) "bad" else "ok")
      val live = scala.collection.mutable.Buffer.empty[Boolean]
      val (answers, _) = session(cmds, live)
      val history = cmds.zip(answers).map { case (c, a) => (c, Some(a)): Seen }
      val audit = History.of(history).map(_.extend(refuse).all).getOrElse(Vector.empty)
      assertEquals(audit, live.toVector, s"seed $seed")
      refused += live.count(identity)
    }
    assert(refused > 100, s"$refused refused")
  }
}

class MachineOverHistory extends OverHistoryLaws(Machine, "machine")
class FreeOverHistory extends OverHistoryLaws(Free, "free")
