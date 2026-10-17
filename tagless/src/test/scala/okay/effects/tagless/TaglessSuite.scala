package okay.effects.tagless

import okay.effects.{Effects, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free
import Tagless._
import Tagless.std._

/** the programs, made once, of no implementation: written for both compilers, the rows by `Sum` */
object Programs1 {
  type Reads[X] = Std.Reader[String, X]
  type Counts[X] = Std.State[Int, X]
  type App[X] = Sum[({ type L[x] = Sum[Void, Reads, x] })#L, Counts, X]

  def greet[R[_]](implicit r: In[Reads, R], s: In[Counts, R]): String ! R =
    ask[String, R].flatMap((name: String) => get[Int, R].flatMap((n: Int) => put[Int, R](n + 1).map(_ => s"$n: $name")))

  val greeted: (Int, String) ! Pure = state(10)(reader("Ann")(greet[App]))

  type Pick[X] = Sum[Void, Std.Choose, X]
  val pairs: List[(Int, Int)] ! Pure =
    all(among[Int, Pick](List(1, 2)).flatMap((a: Int) => among[Int, Pick](List(10, 20)).map((b: Int) => (a, b))))
  val found: Option[Int] ! Pure =
    first(among[Int, Pick](List(1, 2, 3, 4)).flatMap((a: Int) => if (a % 2 == 0) pure[Int, Pick](a) else fail[Int, Pick]))

  type Gen[X] = Sum[Void, ({ type L[x] = Std.Yield[Int, x] })#L, X]
  def nats(n: Int): Unit ! Gen = emit[Int, Gen](n).flatMap(_ => nats(n + 1))
  val firstThree: (List[Int], Option[Unit]) ! Pure = take[Int](3)(nats(0))

  type Fails[X] = Sum[Void, ({ type L[x] = Std.Error[String, x] })#L, X]
  val raised: Either[String, Int] ! Pure = attempt[String](raise[String, Fails, Int]("boom").map(_ + 1))

  type C[X] = Std.Control[Int ! Pure, X]
  type Ctl[X] = Sum[Void, C, X]
  val shifted: Int ! Pure = reset[Int, Pure](shift[Int, Int, Pure, Ctl]((k: Int => Int ! Pure) => k(1).flatMap((a: Int) => k(a))).map(_ * 10))

  def loop(n: Int): Unit ! App =
    if (n == 0) pure[Unit, App](()) else modify[Int, App](_ + 1).flatMap(_ => loop(n - 1))
  val counted: (Int, Unit) ! Pure = state(0)(reader("x")(loop(100000)))
}

abstract class TaglessChecks(fx0: Effects) extends munit.FunSuite {
  implicit val fx: Effects = fx0
  import Programs1._

  test("a program made once, run on the implementation given") { assertEquals(run(greeted), (11, "10: Ann")) }
  test("a general handler resuming k for each choice, and the first found") {
    assertEquals(run(pairs), List((1, 10), (1, 20), (2, 10), (2, 20)))
    assertEquals(run(found), Some(2))
  }
  test("a stateful general handler: the first three of a generator") { assertEquals(run(firstThree), (List(0, 1, 2), None)) }
  test("an aborting handler") { assertEquals(run(raised), Left("boom")) }
  test("shift and reset: k resumed twice") { assertEquals(run(shifted), 100) }
  test("in constant stack: 100 000 operations") { assertEquals(run(counted)._1, 100000) }
}

class TaglessOnMachine extends TaglessChecks(Machine)
class TaglessOnFree extends TaglessChecks(Free)

class TaglessOnBoth extends munit.FunSuite {
  test("one value, two implementations") {
    def onMachine = { implicit val fx: Effects = Machine; run(Programs1.greeted) }
    def onFree = { implicit val fx: Effects = Free; run(Programs1.greeted) }
    assertEquals(onMachine, onFree)
  }
}

/** the interface's suite on tagless, on each implementation */
class InterfaceOnTaglessMachine extends okay.effects.InterfaceSuite(Tagless.on(Machine))
class InterfaceOnTaglessFree extends okay.effects.InterfaceSuite(Tagless.on(Free))
