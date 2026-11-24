package okay.effects

/** programs written on the interface, as Scala 2 writes them: whatever implementation `fx` is. A row by names,
 * taken as its constructor, `#L`; one precedence for all infix types, each `%` in parentheses */
class Programs(val fx: Runner) {
  import fx._
  import fx.std._

  /** row-polymorphic: in any row that has these, at any place */
  def greet[R[_]](implicit r: In[(Reader % String)#L, R], w: In[(Writer % String)#L, R], s: In[(State % Int)#L, R])
    : Int ! R =
    for {
      name <- ask[String, R]
      _ <- tell[String, R](s"hello, $name")
      n <- get[Int, R]
      _ <- put[Int, R](n + name.length)
    } yield n

  type Row[X] = ((Reader % String) + (Writer % String) + (State % Int))#L[X]
  type Reordered[X] = ((State % Int) + (Reader % String) + (Writer % String))#L[X]

  def handledInOrder = run(State.run(10)(Writer.run[String](Reader.run("Ann")(greet[Row]))))
  def handledOutOfOrder = run(Reader.run("Ann")(State.run(10)(Writer.run[String](greet[Row]))))
  def anotherRow = run(Writer.run[String](Reader.run("Ann")(State.run(10)(greet[Reordered]))))

  /** a program of a smaller row, in a bigger one */
  def counter: Int ! (State % Int)#L = get[Int, (State % Int)#L]
  def grown = run(Reader.run("x")(Writer.run[String](State.run(5)(counter.into[Row]))))

  type Choosing[X] = (*[Choose] + (State % Int))#L[X]
  def pick: Int ! Choosing =
    for { x <- among[Int, Choosing](List(1, 2, 3)); _ <- modify[Int, Choosing](_ + x) } yield x * 10
  /** the state outside the choice: one state through every branch */
  def stateOutside = run(State.run(0)(Choose.all(pick)))
  /** the state inside: each branch its own, from the state of its time */
  def stateInside = run(Choose.all(State.run(0)(pick)))

  type Failing[X] = ((Throws % String) + (State % Int))#L[X]
  def safeDiv(a: Int, b: Int): Int ! Failing =
    if (b == 0) raise[String, Failing, Int]("div by zero") else modify[Int, Failing](_ + 1).as(a / b)
  def divided(b: Int) = run(State.run(0)(Throws.run[String](safeDiv(10, b))))
  def stateKeptOnRaise = run(Throws.run[String](State.run(0)(safeDiv(10, 0))))

  /** a split on its own, then a general handler of the effect now last; the rest of a row of two, one taken, is
   * `Pure + E`: `Pure` named `*[Pure]` */
  type RS[X] = ((Reader % String) + (State % Int))#L[X]
  type S1[X] = (*[Pure] + (State % Int))#L[X]
  def asked: Int ! RS = ask[String, RS].flatMap(n => get[Int, RS].map(_ + n.length))
  def splitThenHandle = run(State.run(1)(handleLast(split[(Reader % String)#L, RS, S1, Int](asked),
        new Handler[(Reader % String)#L, S1, Int, Int] {
          def ret(a: Int) = pure(a * 10)
          def op[X](op: Reader[String, X], k: X => Int ! S1) =
            op match { case Std.Reader.Ask(answer) => k(answer("abc")) }
        })))

  /** constant stack: n operations under two handlers, n binds */
  def loop(n: Int): Unit ! Row =
    if (n == 0) pure(()) else ask[String, Row].flatMap(_ => modify[Int, Row](_ + 1)).andThen(loop(n - 1))
  def counted(n: Int) = run(Writer.run[String](Reader.run("x")(State.run(0)(loop(n)))))
  def binds(n: Int) = run((1 to n).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1)))
}
