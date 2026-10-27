package okay.effects

import okay.effects.cek.OnCek

/** AN OKAY-SIMPLE PROGRAM IN AN OKAY-SCALA ROW: its effects found in okay-scala's row in another order, handled there
 * by okay-scala's handlers; its own handler kept inside; one machine, one run */
class IntoSuite extends munit.FunSuite:
  // effects as both libraries perform them: operations as data, each with its answer
  sealed trait Ask[X]
  final case class Get[X](key: String, answer: Option[Int] => X) extends Ask[X]
  sealed trait Note[X]
  final case class Tell[X](line: String, answer: Unit => X) extends Note[X]
  sealed trait Twice[X]
  final case class Of[X](n: Int, answer: Int => X) extends Twice[X]

  // okay-simple's rows: `Void` first, `Sum` an effect more
  type R1[X] = Sum[Void, Ask, X]
  type R2[X] = Sum[R1, Note, X]
  type R3[X] = Sum[R2, Twice, X]

  /** written over okay-simple's interface — on `OnCek`, a program of okay-scala's machine */
  val simple: OnCek.![Int, R2] =
    val inner: OnCek.![Int, R3] =
      OnCek.flatMap(OnCek.perform[Ask, R3, Option[Int]](Get("a", x => x)))(a =>
        OnCek.flatMap(OnCek.perform[Twice, R3, Int](Of(a.getOrElse(0), x => x)))(n =>
          OnCek.map(OnCek.perform[Note, R3, Unit](Tell(s"twice $n", x => x)))(_ => n)))
    // its own handler, okay-simple's, inside
    OnCek.handle[Twice, R3, R2, Int, Int](inner, new OnCek.TailHandler[Twice, R2, Int, Int]:
      def ret(a: Int) = OnCek.pure(a)
      def answer[X](op: Twice[X]): X = op match
        case Of(n, answer) => answer(n * 2))

  test("into okay-scala's row (the effects in another order), handled by okay-scala's handlers") {
    import okay.scala3.{+, !, Handler, handle, pure, run}
    type S[X] = (Note + Ask)[X]
    val q: Int ! S = OnCek.into[S](simple)
    val told = scala.collection.mutable.ListBuffer.empty[String]
    val answered = handle(q, Handler.tail[Ask, Note, Int, Int](pure)([X] => (op: Ask[X]) => op match
      case Get(_, answer) => answer(Some(21))))
    val got = run(handle(answered, Handler.tail[Note, okay.scala3.Pure, Int, Int](pure)([X] => (op: Note[X]) => op match
      case Tell(line, answer) => told += line; answer(()))))
    assertEquals(got, 42)
    assertEquals(told.toList, List("twice 42"))
  }

  test("into a row with more: okay-scala's own effects beside, performed in the same program") {
    import okay.scala3.*
    import okay.scala3.std.{Log, Level}
    type S[X] = (Log + Ask + Note)[X]
    val q: Int ! S = OnCek.into[S](simple).flatMap(n => Log.info[S](s"got $n").as(n))
    val log = scala.collection.mutable.ListBuffer.empty[String]
    val got = run(Log.to[Log, okay.scala3.Pure, Int]((_, m) => log += m)(
      handle(handle(q, Handler.tail[Note, Log + Ask, Int, Int](pure)([X] => (op: Note[X]) => op match
        case Tell(line, answer) => log += line; answer(()))),
        Handler.tail[Ask, Log, Int, Int](pure)([X] => (op: Ask[X]) => op match
          case Get(_, answer) => answer(None)))))
    assertEquals(got, 0)
    assertEquals(log.toList, List("twice 0", "got 0"))
  }
