# algebra — the classes of computations and of combination

`okay-effects-algebra`, `okay.effects.algebra` — `import okay.effects.algebra.*` the module on the machine, and
`import okay.effects.algebra.syntax.*` the generic words and the operations as methods on values. The classes written
once for both compilers, from okay's okay2 (its Scala 2.13 core), the platform's parts left out:

| | |
|---|---|
| computations | `Functor`, `Applicative`, `Selective`, `Monad`, `TailRecM`, `Alternative`, `MonadPlus`, `Comonad`; `ParaMonad` (Atkey), `DiagonalMonad` |
| combination | `Semigroup`, `Monoid`, `Group`; `Fold`, `FoldUntil`, `Foldable` |
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

## What is not here

`okay2`'s `CanFail` (a `withFilter` for programs over `Abort` or `Choose`) and the `Control` instances are the
platform's; the specialised folds (`OfLong`, `OfInt`) wait for a measurement that wants them.
