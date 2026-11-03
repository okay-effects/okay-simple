# algebra — the classes of computations and of combination

`okay-effects-algebra`, `okay.effects.algebra` — `import okay.effects.algebra.*` the module on the machine, and
`import okay.effects.algebra.syntax.*` the generic words and the operations as methods on values. The classes written
once for both compilers, from okay's okay2 (its Scala 2.13 core), the platform's parts left out:

| | |
|---|---|
| computations | `Functor`, `Applicative`, `Selective`, `Monad`, `TailRecM`, `Alternative`, `MonadPlus`, `Comonad`; `ParaMonad` (Atkey), `DiagonalMonad` |
| combination | `Semigroup`, `Monoid`, `Group`; `Fold` (a state of its own, `end`ed to the result; two folds in one pass, an Applicative), `FoldUntil`, `Foldable` |
| arrows | `Profunctor`, `Category`, `Arrow`; instances for functions, `Kleisli` over any Monad, `Fold` a Profunctor; `>>>`, `<<<`, `***`, `&&&`, `dimap` |
| comonads | `History` (the past and a focus), `Env` (a value beside its environment), `Store` (a position in a space), `Cofree[F, A]` (a tree of what may follow; `unfold`, lazy) — each a `Comonad`, its laws property-tested |
| coeffects | `overHistory(p)(kept, decide)`: each operation of an effect decided by a function of the history behind it (`Kept`: an operation before and after it runs; `Decide`: go on, or answered here) — the history the comonad it reads; `History.extend` of the same decision over the history kept is its audit, the decisions made live |
| free | `FreeAp[F, A]` with `foldMap` into any Applicative and `analyze` without running; `Validated[E, A]` accumulating by a Semigroup; `~>`, `Const` |
| instances | `Option`, `Either`, `List`, `Vector`, `LazyList`, `Id` in `Functor`'s companion; every collection a `Foldable`; `String`, collections, `Map`, `Option`, numerics as Monoids |
| programs | `monad[R]`: `Monad` and `TailRecM` of `A ! R`; `monadPlus[R]` with `Choose` in `R`; `paraMonad[R]`: `Delimited`'s `Cps` as a `ParaMonad` |
| words | for programs, the row given: `traverse[A, B, R]`, `sequence`, `replicateA`, `loop` (tailRecM), `*>`, `<*`, `>>`, `void`; generic, on any instance, in `syntax`: the same, and `<*>`, `>>=`, `<|>`, `\|+\|`, `ifS`, `whenS`, `select`, `branch`, `foldWith`, `foldMap` |

```scala
import okay.effects.*
import okay.effects.std.*
import okay.effects.algebra.*
import okay.effects.algebra.syntax.{FoldableSyntax, SemigroupSyntax}

// programs are a Monad: traverse over them, loop stack-safely — the words shaped for programs take the row
assert(run(traverse[Int, Int, Pure](List(1, 2, 3))(n => pure[Int, Pure](n * 2))) == Vector(2, 4, 6))
assert(run(loop[Int, Int, Pure](0)(n => pure[Either[Int, Int], Pure](if (n < 100000) Left(n + 1) else Right(n)))) == 100000)
assert(run(pure[Int, Pure](1) *> pure[Int, Pure](2)) == 2)
assert(run(monad[Pure].flatMap(pure[Int, Pure](1))(n => pure[Int, Pure](n + 1))) == 2)                       // the instance, by name

// a row with Choose is a MonadPlus: append is a choice, empty fails
val P = monadPlus[Choose]
assert(run(first(P.flatMap(P.append(pure[Int, Choose](1), pure[Int, Choose](2)))(n => P.flatMap(P.guard(n > 1))(_ => P.pure(n))))) == Some(2))

// the standard types, and the structures
assert(Monad[Option].flatMap(Some(2))(n => Some(n + 1)) == Some(3))
assert(syntax.traverse(List(1, 2))(n => Option(n + 1)) == Some(Vector(2, 3)))                                   // the generic words, on any instance
assert(Monoid[Map[String, Int]].combine(Map("a" -> 1), Map("a" -> 2, "b" -> 1)) == Map("a" -> 3, "b" -> 1))
assert(List(1, 2, 3).foldWith(Fold.sum[Int]) == 6)
assert(Foldable[LazyList].foldUntil(LazyList.from(1))(FoldUntil.take(3)) == Vector(1, 2, 3))
```

## The free applicative

Steps that do not depend on one another's values: run in any Applicative, or inspected before any is run. A record's
fields are the example — `shape`'s `Fields` — decoded into a `Validated` that keeps every problem, listed into a schema
without decoding anything:

```scala
sealed trait Field[A]
final case class Text(name: String) extends Field[String]
final case class Number(name: String) extends Field[Int]
final case class Person(name: String, age: Int)

val person: FreeAp[Field, Person] =
  FreeAp.Ap(FreeAp.Ap(FreeAp.pure[Field, String => Int => Person](n => a => Person(n, a)), FreeAp.lift(Text("name"))), FreeAp.lift(Number("age")))

val names = person.analyze(new Const[Field, List[String]] { def apply[A](f: Field[A]) = f match { case Text(n) => List(n); case Number(n) => List(n) } })
assert(names == List("name", "age"))

type V[A] = Validated[List[String], A]
val nothing = new (Field ~> V) { def apply[A](f: Field[A]): V[A] = f match { case Text(n) => Validated.Invalid(List(s"$n missing")); case Number(n) => Validated.Invalid(List(s"$n missing")) } }
assert(person.foldMap[V](nothing) == Validated.Invalid(List("name missing", "age missing")))
```

## A parameterised monad

`Delimited`'s `Cps[A, B, C, R]` — a program of `A` whose answer `B` is changed to `C` — is Atkey's parameterised monad
`M[A, S, T]` with `S = B`, `T = C`: `pure` keeps the answer, `flatMap` composes the changes. `paraMonad[R]` is the
instance; `ParaMonad.diagonal` the ordinary Monad on one answer type.

## Arrows

Functions, effectful functions (`Kleisli` over a Monad, programs' among them) and folds compose as arrows: `>>>`, side
by side `***`, on one input `&&&`. A `Fold` is a profunctor — what goes in, what comes out — mapped on either side.
Optics by profunctors are not here: the simple optics fall out of `shape` (a field a lens, a case a prism) and come as
their own module when an application edits a nested record.

```scala
import okay.effects.algebra.syntax.ArrowSyntax
val f: Int => Int = _ + 1
val g: Int => String = _.toString
assert((f >>> g)(1) == "2" && (f &&& g)(1) == (2, "1"))
val half = Arrow.Kleisli[Option, Int, Int](n => if (n % 2 == 0) Some(n / 2) else None)
assert((half >>> half).run(8) == Some(2) && (half >>> half).run(6) == None)
```

## What is not here

`okay2`'s `CanFail` (a `withFilter` for programs over `Abort` or `Choose`) and the `Control` instances are the
platform's; the specialised folds (`OfLong`, `OfInt`) wait for a measurement that wants them.

## Coeffects: what a computation needs from its context

Effects say what a program does to the world; coeffects (Petricek, Orchard and Mycroft, 2014) what it needs from its
context — a comonad's. A flat coeffect (a value read anywhere: a setting, the user) is an effect already: an operation
a handler answers. One that needs the PAST is a comonad's: `overHistory` keeps the history of an effect's operations
and decides each by a function of it.

```scala
sealed trait Cmd[X]
final case class Exec(cmd: String) extends Cmd[Boolean]
type Seen = (String, Option[Boolean])                      // a command, and how it went (none while pending)
val kept = new Kept[Cmd, Seen] {
  def pending[X](op: Cmd[X]): Seen = op match { case Exec(c) => (c, None) }
  def ended[X](op: Cmd[X], ok: X): Seen = op match { case Exec(c) => (c, Some(ok)) }
}
def stuck(h: History[Seen]): Boolean = h.past.reverseIterator.takeWhile(_._2.contains(false)).size >= 3
val refuseWhenStuck = new Decide[Cmd, Seen] {             // refused after three failures in a row
  def apply[X](h: History[Seen], op: Cmd[X]): Option[X] = op match { case Exec(_) => if (stuck(h)) Some(false) else None }
}
var ran = 0
val commands = traverse[String, Boolean, Cmd](Vector("bad", "bad", "bad", "ls"))(c => perform[Cmd].apply[Cmd, Boolean](Exec(c)))
val shell = new TailClauses[Cmd, Vector[Boolean], Vector[Boolean] ! Pure] {
  def ret(a: Vector[Boolean]) = pure[Vector[Boolean], Pure](a)
  def answer[X](op: Cmd[X]): X = op match { case Exec(c) => ran += 1; c != "bad" }
}
val answers = run(handle(overHistory(commands)(kept, refuseWhenStuck), shell))
assert(answers == Vector(false, false, false, false) && ran == 3)  // the fourth refused: the shell never asked
// the audit: the same decision at every step of the history kept
val journal = Vector("bad", "bad", "bad", "ls").zip(answers).map { case (c, a) => (c, Some(a)): Seen }
assert(History.of(journal).map(_.extend(stuck).all) == Some(Vector(false, false, false, true)))
```

The law (`OverHistorySuite`, on the machine and on Free, 300 random sessions): the audit by `extend` over the history
kept gives exactly the decisions made live.

