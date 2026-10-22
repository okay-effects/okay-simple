package okay.effects.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{
  Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State => JmhState
}
import okay.effects.{Runner, Std}

/** SHAPES OF `first`, side by side in one JVM, on the same searches: where its cost is. `old` the nested frames of
 * before the cut (no cut); `std` the library's; `fused` a stateful handler whose pending alternative is one node;
 * `handler` a plain handler, the stack passed in the answer.
 * `sbt "bench/Jmh/run -f 1 -wi 3 -i 5 FirstVariantsBenchmark"` */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class FirstVariantsBenchmark {
  @Param(Array("machine", "free"))
  var impl: String = ""
  @Param(Array("old", "std", "fused", "handler"))
  var shape: String = ""

  var search: () => Option[Int] = null
  var chain: () => Option[Int] = null

  @Setup def setup(): Unit = {
    val fx: Runner = if (impl == "machine") okay.effects.machine.Machine else okay.effects.free.Free
    import fx._
    import fx.std._
    type B = Option[Int] ! Pure

    def old(p: Int ! Choose): B =
      handle[Choose, Choose, Pure, Int, Option[Int]](p, new Handler[Choose, Pure, Int, Option[Int]] {
        def ret(a: Int) = pure(Some(a))
        def op[X](op: Choose[X], k: X => B) = op match {
          case Std.Choose.Among(xs) =>
            def go(ys: List[X]): B =
              if (ys.isEmpty) pure[Option[Int], Pure](None)
              else if (ys.tail.isEmpty) k(ys.head)
              else k(ys.head).flatMap { case None => go(ys.tail); case found => pure[Option[Int], Pure](found) }
            go(xs)
          case _ => pure[Option[Int], Pure](None)
        }
      })

    /** one node per pending alternative: the continuation and the ways left, the node below, the depth */
    final class Node(val k: (Any, Node) => B, val ys: List[Any], val below: Node, val depth: Int)
    def fused(p: Int ! Choose): B =
      handle[Choose, Node, Choose, Pure, Int, Option[Int]](p, null,
        new StatefulHandler[Choose, Node, Pure, Int, Option[Int]] {
          def ret(s: Node, a: Int) = pure(Some(a))
          def depth(s: Node): Int = if (s eq null) 0 else s.depth
          def back(s: Node): B =
            if (s eq null) pure[Option[Int], Pure](None)
            else {
              val t = s.ys.tail
              if (t.isEmpty) s.k(s.ys.head, s.below) else s.k(s.ys.head, new Node(s.k, t, s.below, s.depth))
            }
          def op[X](s: Node, op: Choose[X], k: (X, Node) => B) = op match {
            case Std.Choose.Among(xs) =>
              if (xs.isEmpty) back(s)
              else if (xs.tail.isEmpty) k(xs.head, s)
              else k(xs.head, new Node(k.asInstanceOf[(Any, Node) => B], xs.tail, s, depth(s) + 1))
            case Std.Choose.Mark(answer) => k(answer(depth(s)), s)
            case Std.Choose.Cut(m, answer) =>
              var t = s; while ((t ne null) && t.depth > m) t = t.below
              k(answer(()), t)
          }
        })

    type F = Node2 => B
    final class Node2(val alt: () => F, val below: Node2, val depth: Int)
    def handler(p: Int ! Choose): B = {
      def back(s: Node2): B = if (s eq null) pure[Option[Int], Pure](None) else s.alt()(s.below)
      handle[Choose, Choose, Pure, Int, F](p, new Handler[Choose, Pure, Int, F] {
        def ret(a: Int) = pure[F, Pure]((_: Node2) => pure[Option[Int], Pure](Some(a)))
        def op[X](op: Choose[X], k: X => F ! Pure) = op match {
          case Std.Choose.Among(xs) =>
            def go(ys: List[X]): F = (s: Node2) =>
              if (ys.isEmpty) back(s)
              else if (ys.tail.isEmpty) k(ys.head).flatMap(_(s))
              else k(ys.head).flatMap(_(new Node2(() => go(ys.tail), s, if (s eq null) 1 else s.depth + 1)))
            pure[F, Pure](go(xs))
          case Std.Choose.Mark(answer) => pure[F, Pure]((s: Node2) =>
              k(answer(if (s eq null) 0 else s.depth)).flatMap(_(s)))
          case Std.Choose.Cut(m, answer) => pure[F, Pure]((s: Node2) => {
              var t = s; while ((t ne null) && t.depth > m) t = t.below
              k(answer(())).flatMap(_(t))
            })
        }
      }).flatMap(_(null))
    }

    val run1: (Int ! Choose) => Option[Int] = shape match {
      case "old" => p => run(old(p))
      case "std" => p => run(first(p))
      case "fused" => p => run(fused(p))
      case _ => p => run(handler(p))
    }
    def bits(n: Int, acc: Int): Int ! Choose =
      if (n == 0) pure[Int, Choose](acc)
      else among[Int, Choose](List(0, 1)).flatMap(b => bits(n - 1, acc * 2 + b))
    def ones(n: Int): Int ! Choose =
      bits(n, 0).flatMap(v => if (v == (1 << n) - 1) pure[Int, Choose](v) else fail[Int, Choose])
    def single(n: Int, acc: Int): Int ! Choose =
      if (n == 0) pure[Int, Choose](acc) else among[Int, Choose](List(1)).flatMap(x => single(n - 1, acc + x))
    // the answers checked once, so a wrong shape does not measure fast
    assert(run1(ones(10)) == Some(1023) && run1(single(1000, 0)) == Some(1000))
    search = () => run1(ones(16))
    chain = () => run1(single(100000, 0))
  }

  @Benchmark def firstSearch: Option[Int] = search()
  @Benchmark def firstChain: Option[Int] = chain()
}
