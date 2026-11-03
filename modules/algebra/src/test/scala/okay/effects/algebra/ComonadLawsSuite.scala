package okay.effects.algebra.checks

import okay.effects.algebra.{Comonad, Cofree, Env, History, Store}
import scala.util.Random

/** THE COMONAD LAWS, for each instance, over many values: extract after coflatMap is f; coflatMap of extract changes
 * nothing; coflatMap associates */
class ComonadLawsSuite extends munit.FunSuite {
  def laws[W[_], A](name: String, samples: Seq[W[A]], f: W[A] => Int, g: W[A] => Int, h: W[Int] => Int,
      same: (W[Int], W[Int]) => Boolean, sameA: (W[A], W[A]) => Boolean)(implicit W: Comonad[W]): Unit =
    samples.zipWithIndex.foreach { case (w, i) =>
      assertEquals(W.extract(W.coflatMap(w)(f)), f(w), s"$name $i: extract ∘ coflatMap f = f")
      assert(sameA(W.coflatMap(w)(W.extract[A]), w), s"$name $i: coflatMap extract = id")
      assert(same(W.coflatMap(W.coflatMap(w)(g))(h), W.coflatMap(w)(x => h(W.coflatMap(x)(g)))),
        s"$name $i: coflatMap associates")
    }

  test("History: the laws over 300 histories") {
    val r = new Random(1)
    val hs = (1 to 300).map(_ => History(Vector.fill(r.nextInt(8))(r.nextInt(10)), r.nextInt(10)))
    laws[History, Int]("History", hs, h => h.past.sum * 3 + h.focus, h => h.past.size - h.focus,
      h => h.past.length + h.focus, _ == _, _ == _)
  }

  test("Env: the laws") {
    type E[X] = Env[String, X]
    val es: Seq[E[Int]] = (1 to 50).map(n => Env("ctx" * (n % 3), n))
    laws[E, Int]("Env", es, e => e.env.length + e.value, e => e.value * 2, e => e.value - e.env.length, _ == _, _ == _)
  }

  test("Store: the laws, compared at positions around") {
    type S[X] = Store[Int, X]
    val ss: Seq[S[Int]] = (0 to 30).map(p => Store((i: Int) => i * i - p, p))
    def same[X](a: S[X], b: S[X]): Boolean = a.pos == b.pos && (-5 to 40).forall(i => a.peek(i) == b.peek(i))
    laws[S, Int]("Store", ss, s => s.peek(s.pos - 1) + s.extract, s => s.peek(s.pos + 1),
      s => s.extract + s.peek(s.pos - 2), same[Int], same[Int])
  }

  test("Cofree: the laws over branching trees, compared to a depth; f at every subtree") {
    type T[X] = Cofree[List, X]
    def tree(seed: Int): T[Int] = Cofree.unfold[List, Int](seed)(n => if (n > 40) Nil else List(n * 2, n * 3 + 1))
    def flat[X](t: T[X], depth: Int): List[X] = if (depth == 0) List(t.head) else t.head :: t.tail.flatMap(flat(_, depth - 1))
    def same[X](a: T[X], b: T[X]): Boolean = flat(a, 6) == flat(b, 6)
    def size(t: T[Int]): Int = 1 + t.tail.map(size).sum
    laws[T, Int]("Cofree", (1 to 20).map(tree), t => t.head + t.tail.size, size, t => t.head * 2 + size(t), same[Int],
      same[Int])
    // the risk below each point: the count of subtrees whose head is over 30
    def risky(t: T[Int]): Int = (if (t.head > 30) 1 else 0) + t.tail.map(risky).sum
    val annotated = Cofree.comonad[List].coflatMap(tree(1))(risky)
    assertEquals(annotated.head, risky(tree(1)))
    assertEquals(annotated.tail.map(_.head), tree(1).tail.map(risky))
  }
}
