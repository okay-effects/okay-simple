package okay.effects.algebra.checks

import okay.effects.algebra.{Algebra, Comonad, Const, Fold, FoldUntil, Foldable, FreeAp, Id, Monad, MonadPlus, Monoid, ParaMonad, TailRecM, Validated, ~>}
import okay.effects.algebra.syntax.{loop, replicateA, sequence, traverse, FoldableSyntax, SemigroupSyntax}
import okay.effects.{Runner, Std, Sum, Void}
import okay.effects.machine.Machine
import okay.effects.free.Free

/** THE LAWS, on a carrier `F` given its instances and a way to compare two `F[A]`s by their values */
abstract class Laws[F[_]](name: String)(implicit M: Monad[F]) extends munit.FunSuite {
  def eq[A](x: F[A], y: F[A]): Unit
  def some(n: Int): F[Int]

  val f: Int => Int = _ + 1
  val g: Int => Int = _ * 2
  val kf: Int => F[Int] = n => M.pure(n + 1)
  val kg: Int => F[Int] = n => M.pure(n * 2)

  test(s"$name: functor identity and composition") {
    eq(M.fmap(some(3), (x: Int) => x), some(3))
    eq(M.fmap(M.fmap(some(3), f), g), M.fmap(some(3), f.andThen(g)))
  }
  test(s"$name: applicative identity, homomorphism, interchange") {
    eq(M.app(M.pure((x: Int) => x), some(3)), some(3))
    eq(M.app(M.pure(f), M.pure(3)), M.pure(f(3)))
    eq(M.app(M.pure(f), M.pure(3)), M.app(M.pure((h: Int => Int) => h(3)), M.pure(f)))
  }
  test(s"$name: monad left and right identity, associativity") {
    eq(M.flatMap(M.pure(3))(kf), kf(3))
    eq(M.flatMap(some(3))(M.pure), some(3))
    eq(M.flatMap(M.flatMap(some(3))(kf))(kg), M.flatMap(some(3))(a => M.flatMap(kf(a))(kg)))
  }
  test(s"$name: selective skips the branch not taken") {
    var ran = false
    val skipped: F[Int] = M.select(M.fmap(some(3), (n: Int) => Right(n): Either[Int, Int]), { ran = true; M.pure((n: Int) => n * 100) })
    eq(skipped, some(3))
    assert(!ran)
    eq(M.select(M.fmap(some(3), (n: Int) => Left(n): Either[Int, Int]), M.pure((n: Int) => n * 100)), some(300))
    eq(M.ifS(M.pure(true))(some(1))(some(2)), some(1))
  }
  test(s"$name: traverse and sequence") {
    eq(M.fmap(traverse(List(1, 2, 3))(kf), (v: Vector[Int]) => v.sum), M.pure(9))
    eq(M.fmap(sequence(List(some(1), some(2))), (v: Vector[Int]) => v.toList), M.fmap(some(1), (a: Int) => List(a, a + 1)))
    eq(M.fmap(replicateA(3)(some(2)), (v: Vector[Int]) => v.size), M.pure(3))
  }
}

class OptionLaws extends Laws[Option]("Option") {
  def eq[A](x: Option[A], y: Option[A]): Unit = assertEquals(x, y)
  def some(n: Int): Option[Int] = Some(n)
  test("tailRecM counts to a hundred thousand without a frame per step") {
    assertEquals(loop[Option, Int, Int](0)(n => if (n < 100000) Some(Left(n + 1)) else Some(Right(n))), Some(100000))
  }
}
class EitherLaws extends Laws[({ type L[X] = Either[String, X] })#L]("Either") {
  def eq[A](x: Either[String, A], y: Either[String, A]): Unit = assertEquals(x, y)
  def some(n: Int): Either[String, Int] = Right(n)
}
class ListLaws extends Laws[List]("List") {
  def eq[A](x: List[A], y: List[A]): Unit = assertEquals(x, y)
  def some(n: Int): List[Int] = List(n)
  test("MonadPlus: empty, append, guard; tailRecM depth-first") {
    val P = MonadPlus[List]
    assertEquals(P.append(List(1), List(2)), List(1, 2))
    assertEquals(P.flatMap(List(1, 2, 3))(n => P.flatMap(P.guard(n % 2 == 1))(_ => P.pure(n))), List(1, 3))
    assertEquals(loop[List, Int, Int](0)(n => if (n < 3) List(Left(n + 1), Right(n)) else List(Right(n))), List(3, 2, 1, 0))
  }
}
class LazyListLaws extends Laws[LazyList]("LazyList") {
  def eq[A](x: LazyList[A], y: LazyList[A]): Unit = assertEquals(x.toList, y.toList)
  def some(n: Int): LazyList[Int] = LazyList(n)
  test("tailRecM is lazy and deep") {
    val deep = loop[LazyList, Int, Int](0)(n => if (n < 100000) LazyList(Left(n + 1)) else LazyList(Right(n)))
    assertEquals(deep.toList, List(100000))
    val infinite = loop[LazyList, Int, Int](0)(n => LazyList(Right(n), Left(n + 1)))
    assertEquals(infinite.take(3).toList, List(0, 1, 2))
  }
}
class IdLaws extends Laws[Id]("Id") {
  def eq[A](x: A, y: A): Unit = assertEquals(x, y)
  def some(n: Int): Int = n
  test("Comonad[Id]") { assertEquals(Comonad[Id].extract(3), 3); assertEquals(Comonad[Id].coflatMap(3)(_ + 1), 4) }
}

/** programs, on an implementation: compared by running */
abstract class ProgramLaws(val fx0: Runner, name: String) extends munit.FunSuite {
  val alg: Algebra[fx0.type] = new Algebra[fx0.type]()(fx0)
  import fx0._
  import fx0.std._
  type P[X] = Sum[Void, Std.Choose, X]
  implicit val monad: Monad[({ type L[A] = A ! Void })#L] with TailRecM[({ type L[A] = A ! Void })#L] = alg.monad[Void]
  /** not implicit beside `monad`: an implementation's program type may not tell its rows apart (the row a phantom), and
   * then the two would be ambiguous — the MonadPlus is asked for by name */
  val plus: MonadPlus[({ type L[A] = A ! P })#L] = alg.monadPlus[P]

  class OnPure extends Laws[({ type L[A] = A ! Void })#L](s"$name programs") {
    def eq[A](x: A ! Void, y: A ! Void): Unit = assertEquals(alg.fx.run(x), alg.fx.run(y))
    def some(n: Int): Int ! Void = pure[Int, Void](n)
  }
  val pureLaws = new OnPure
  pureLaws.munitTests().foreach(t => test(t.name)(t.body()))

  test(s"$name: tailRecM over programs is a loop, a hundred thousand deep") {
    assertEquals(alg.fx.run(alg.loop[Int, Int, Void](0)(n => if (n < 100000) pure[Either[Int, Int], Void](Left(n + 1)) else pure[Either[Int, Int], Void](Right(n)))), 100000)
  }
  test(s"$name: MonadPlus over a row with Choose: append is a choice, empty fails") {
    val both: Int ! P = plus.append(pure[Int, P](1), pure[Int, P](2))
    assertEquals(alg.fx.run(first(both)), Some(1))
    assertEquals(alg.fx.run(first(plus.flatMap(plus.append(pure[Int, P](1), pure[Int, P](2)))(n => plus.flatMap(plus.guard(n > 1))(_ => plus.pure(n))))), Some(2))
    assertEquals(alg.fx.run(first(plus.empty[Int])), None)
  }
  test(s"$name: the words shaped for programs, the implementation abstract") {
    import alg.ProgramOps
    val p: Int ! Void = pure[Int, Void](1) *> pure[Int, Void](2)
    assertEquals(alg.fx.run(p), 2)
    assertEquals(alg.fx.run(pure[Int, Void](1) <* pure[Int, Void](2)), 1)
    assertEquals(alg.fx.run(alg.traverse[Int, Int, Void](List(1, 2))(n => pure[Int, Void](n * 10))), Vector(10, 20))
    assertEquals(alg.fx.run(alg.sequence[Int, Void](List(pure[Int, Void](1), pure[Int, Void](2)))), Vector(1, 2))
  }
  test(s"$name: Delimited's Cps is a ParaMonad, its diagonal a Monad") {
    val d: alg.fx.delimited.type = alg.fx.delimited
    val pm: ParaMonad[({ type L[A, S, T] = d.Cps[A, S, T, Void] })#L] = alg.paraMonad[Void]
    val m = ParaMonad.diagonal[({ type L[A, S, T] = d.Cps[A, S, T, Void] })#L, Int](pm)
    assertEquals(alg.fx.run(d.reset[Int, Int, Void](m.flatMap(m.pure(3))(n => m.pure(n * 2)))), 6)
    // the answer type changed: a shift that returns a string where an int was expected
    val changed: d.Cps[Int, Int, String, Void] = pm.flatMap[Int, Int, Int, Int, String](d.shift[Int, Int, String, Void](k => k(1).map(n => s"got $n")))(n => pm.pure[Int, Int](n + 1))
    assertEquals(alg.fx.run(d.reset[Int, String, Void](changed)), "got 2")
  }
}
class MachineLaws extends ProgramLaws(Machine, "machine")
class FreeLaws extends ProgramLaws(Free, "free")

class StructuresSuite extends munit.FunSuite {
  test("Monoid and Semigroup instances and |+|") {
    assertEquals(Monoid[List[Int]].combineAll(List(List(1), List(2, 3))), List(1, 2, 3))
    assertEquals(Monoid[Map[String, Int]].combine(Map("a" -> 1, "b" -> 1), Map("a" -> 2)), Map("a" -> 3, "b" -> 1))
    assertEquals(Monoid[Option[String]].combineAll(List(Some("a"), None, Some("b"))), Some("ab"))
    assertEquals(Monoid[Int].combine(2, 3), 5)
    assertEquals(Monoid.numeric[Int].inverse(2), -2)
    assertEquals("a" |+| "b", "ab")
  }
  test("Foldable over collections and Option: folds that stop, stop") {
    val F = Foldable[List]
    assertEquals(F.foldMap(List(1, 2, 3))(_.toString), "123")
    assertEquals(F.size(List(1, 2, 3)), 3L)
    assertEquals(F.find(List(1, 2, 3))(_ > 1), Some(2))
    assertEquals(Foldable[LazyList].foldUntil(LazyList.from(1))(FoldUntil.take(3)), Vector(1, 2, 3))
    assertEquals(Foldable[Option].toList(Some(1)), List(1))
    assertEquals(List(1, 2, 3).foldWith(Fold.sum[Int]), 6)
    assertEquals(List(1, 2, 3).foldWith(Fold.last[Int]), Some(3))
  }
  test("FreeAp: run in an applicative, inspected without running") {
    sealed trait Field[A]; final case class Text(name: String) extends Field[String]; final case class Number(name: String) extends Field[Int]
    final case class Person(name: String, age: Int)
    val person: FreeAp[Field, Person] = FreeAp.Ap(FreeAp.Ap(FreeAp.pure[Field, String => Int => Person](n => a => Person(n, a)), FreeAp.lift(Text("name"))), FreeAp.lift(Number("age")))
    assertEquals(person.analyze(new Const[Field, List[String]] { def apply[A](f: Field[A]) = f match { case Text(n) => List(n); case Number(n) => List(n) } }), List("name", "age"))
    val values = Map("name" -> "Ann", "age" -> "41")
    type V[A] = Validated[List[String], A]
    val read = new (Field ~> V) {
      def apply[A](f: Field[A]): V[A] = f match {
        case Text(n) => values.get(n).fold[V[A]](Validated.Invalid(List(s"$n missing")))(Validated.Valid(_))
        case Number(n) => values.get(n).flatMap(_.toIntOption).fold[V[A]](Validated.Invalid(List(s"$n not a number")))(Validated.Valid(_))
      }
    }
    assertEquals(person.foldMap[V](read), Validated.Valid(Person("Ann", 41)))
    val empty = new (Field ~> V) { def apply[A](f: Field[A]): V[A] = f match { case Text(n) => Validated.Invalid(List(s"$n missing")); case Number(n) => Validated.Invalid(List(s"$n missing")) } }
    assertEquals(person.foldMap[V](empty), Validated.Invalid(List("name missing", "age missing")))
    assertEquals(person.foldMap[Option](new (Field ~> Option) { def apply[A](f: Field[A]): Option[A] = f match { case Text(_) => Some("x"); case Number(_) => Some(1) } }), Some(Person("x", 1)))
  }
}
