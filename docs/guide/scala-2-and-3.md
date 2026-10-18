# Scala 2 and Scala 3

One interface for both compilers: `Effectful`, one source. What differs is only how a row is WRITTEN — each
compiler's `Effects`:

| | Scala 3 | Scala 2.13 |
|---|---|---|
| an effect with a parameter | `State % Int` | `(State % Int)#L` |
| an effect of no parameter | `Choose` | `*[Choose]#L` |
| a row | `Reader % String + State % Int` | `((Reader % String) + (State % Int))#L` |
| a requirement | `R[_]: Has[Reader % String]` | `implicit r: In[(Reader % String)#L, R]` |
| a program's methods | extensions | `ProgramOps`, an implicit class |

Scala 2 has no type alias whose result is a type constructor, so a row is written by NAMES (`%`, `+`, `*` are
classes there) and taken as the constructor the names make, `#L`. All infix types share one precedence in Scala 2:
put each `%` in parentheses.

```scala2
import okay.effects._
import okay.effects.std._

object TwoEffects {
  def greet[R[_]](implicit r: In[(Reader % String)#L, R], w: In[(Writer % String)#L, R]): Int ! R =
    for { name <- ask[String, R]; _ <- tell[String, R](s"hello, $name") } yield name.length

  type Row[X] = ((Reader % String) + (Writer % String))#L[X]
  assert(run(writer[String](reader("Ann")(greet[Row]))) == ((List("hello, Ann"), 3)))
}
assert(TwoEffects.toString.nonEmpty)
```

## An effect for both compilers

Scala 2 does not refine a method's type parameter in a match on a GADT, so an effect written for both gives each
operation its answer as a function:

```scala2
sealed trait Ticket[X]
object Ticket { final case class Next[X](answer: Int => X) extends Ticket[X] }

object Tickets {
  import okay.effects._
  def next[R[_]](implicit in: In[Ticket, R]): Int ! R = perform[Ticket].apply[R, Int](Ticket.Next(n => n))

  def tickets[R[_], Rest[_], A](p: A ! R)(implicit s: Split[Ticket, R, Rest]): A ! Rest =
    handle[Ticket, Int, R, Rest, A, A](p, 0, new StatefulTailHandler[Ticket, Int, Rest, A, A] {
      def ret(n: Int, a: A) = pure(a)
      def answer[X](cell: Cell[Int], op: Ticket[X]): X = op match {
        case Ticket.Next(answer) => cell.set(cell.get + 1); answer(cell.get)
      }
    })

  assert(run(tickets(next[Ticket].flatMap(a => next[Ticket].map(b => (a, b))))) == ((1, 2)))
}
assert(Tickets.toString.nonEmpty)
```

## Code written once for both

Shared source writes a row by the aliases both compilers read: `Sum[R, E, X]` is `R + E`, `Void` is `Pure`, and a
parametrised effect `({ type L[x] = State[S, x] })#L`. Requirements as `implicit` parameters, not context bounds.
The interface itself, the standard effects, the suite and every module are written so.

## One application, two compilers

An application with Scala 2.13 code in it takes the interface's Scala 2.13 build, which Scala 3 reads: one build of
the interface in one application. Scala 3 code beside it writes rows by the shared aliases. The repository's
`compat/` and `examples/mixed` check it: an effect, its handler and a program written in Scala 2.13, used by Scala 3
code in one row, one run.
