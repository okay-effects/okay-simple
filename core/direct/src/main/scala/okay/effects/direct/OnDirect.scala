package okay.effects.direct

import okay.effects.{Cell, Runner, Sum}
import okay.direct.{Answer, Body, MultiResume, Resume}

/**
 * THE INTERFACE ON OKAY-DIRECT: okay-effects' programs run by okay-direct's handlers, on any of its implementations
 * — Loom's continuations, threads, Espresso's (multi-shot). A program is a trampoline run with an ENVIRONMENT: the
 * capabilities of okay-direct of the effects of its row, by place, the last effect first. So:
 *
 *  - `perform` takes the capability at its effect's place and performs the operation through it;
 *  - a handler of `E` is one of okay-direct's: its body run with `E`'s capability in front of the environment, its
 *    clause's `k` a program that resumes the body when it runs — while the clause runs, as okay-direct asks;
 *  - `split`, `embed`, `interposed` move capabilities in the environment: no program is rewritten;
 *  - a handler resuming `k` more than once (`Choose`) needs a multi-shot implementation, an `okay.direct.MultiEffects`;
 *    on a one-shot one the second resumption is refused.
 *
 * A handler's `k` lives while its clause's program runs: a clause returning a program that resumes `k` later, after
 * it has returned (as `lower` into a lazy monad does), is not run here.
 *
 * The interface's suite (`InterfaceSuite`): on Loom and on threads 20 of 30, the 10 others multi-shot (refused:
 * "resumed twice"); on Espresso 28 of 30. The two left are in constant stack — `intercept` over 100 000 operations, a
 * million choices cut under `first`: a general handler (an interceptor, `Choose`'s) nests the stack at each resumption
 * in okay-direct (on Espresso, each a copy of the rest too) — tail and stateful handlers run in a loop, a general one
 * does not.
 */
class OnDirect(val effects: okay.direct.Effects) extends Runner {
  import OnDirect._

  type ![+A, R[_]] = Prog[A]
  type In[E[_], R[_]] = Place[E, R]
  type Split[E[_], R[_], Rest[_]] = Cut[E, R, Rest]
  type Sub[R[_], S[_]] = Within[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = new Cut[E, E, Pure](0)
  implicit def splitLast[R[_], E[_]]: Split[E, ({ type L[x] = Sum[R, E, x] })#L, R] =
    new Cut[E, ({ type L[x] = Sum[R, E, x] })#L, R](0)
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]):
      Split[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L] =
    new Cut[E, ({ type L[x] = Sum[R, G, x] })#L, ({ type L[x] = Sum[Rest, G, x] })#L](s.offset + 1)
  implicit def subPure[S[_]]: Sub[Pure, S] = new Within[Pure, S](_ => Nil)
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = new Within[E, S](env => List(env(e.offset)))
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[({ type L[x] = Sum[R, E, x] })#L, S] =
    new Within[({ type L[x] = Sum[R, E, x] })#L, S](env => env(e.offset) :: r.f(env))

  def pure[A, R[_]](a: A): A ! R = Done(a)
  def flatMap[A, B, R[_]](p: A ! R)(f: A => B ! R): B ! R = Bind(p, f)
  def delay[A, R[_]](p: => A ! R): A ! R = Suspend(() => p)

  protected[effects] def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): X ! R = Perf(in.offset, Op[E, X](op))

  def split[E[_], R[_], Rest[_], A](p: A ! R)(implicit s: Split[E, R, Rest]): A ! ({ type L[x] = Sum[Rest, E, x] })#L =
    Local(env => insert(env.tail, s.offset, env.head), p)
  def embed[R[_], S[_], A](p: A ! R)(implicit s: Sub[R, S]): A ! S = Local(s.f, p)
  protected[effects] def interposed[E[_], R[_], A](p: A ! R)(implicit in: In[E, R]): A ! ({ type L[x] = Sum[R, E, x] })#L =
    Local(env => env.tail.updated(in.offset, env.head), p)

  def run[A](p: A ! Pure): A = exec(p, Nil).asInstanceOf[A]

  private def body[E[_], A](p: Prog[A], env: Env): Body[Fam[E], A, RuntimeException] =
    new Body[Fam[E], A, RuntimeException] {
      def run(capability: okay.direct.Effect[Fam[E]]): A = exec(p, capability :: env).asInstanceOf[A]
    }

  /** a general handler: one-shot on a one-shot implementation, multi-shot on a multi-shot one */
  private def general[E[_], A, B](p: Prog[A], env: Env, onValue: A => B,
      clause: (Op[E, Any], Any => B) => B): B = effects match {
    case multi: okay.direct.MultiEffects =>
      multi.handle(new okay.direct.MultiHandler[Fam[E], A, B, RuntimeException] {
        def value(a: A): B = onValue(a)
        def operation(op: Fam[E], k: MultiResume[Fam[E], B, RuntimeException]): B = resumed(op, k, clause)
      }, body[E, A](p, env))
    case one =>
      one.handle(new okay.direct.Handler[Fam[E], A, B, RuntimeException] {
        def value(a: A): B = onValue(a)
        def operation(op: Fam[E], k: Resume[Fam[E], B, RuntimeException]): B = resumed(op, k, clause)
      }, body[E, A](p, env))
  }

  private def resumed[E[_], B](op: Fam[E], k: Resume[Fam[E], B, RuntimeException], clause: (Op[E, Any], Any => B) => B): B =
    op match {
      case o: Op[E, Any] @unchecked => clause(o, (x: Any) => k.resume(o, x))
    }

  def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: Handler[E, R, A, B]): B ! R =
    Act(env => general[E, A, B](p, env, a => exec(h.ret(a), env).asInstanceOf[B],
      (o, k) => exec(h.op[Any](o.op, (x: Any) => Act[B](_ => k(x))), env).asInstanceOf[B]))

  /** answered in place: okay-direct's tail handler, in constant stack */
  override def handleLast[E[_], R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, h: TailHandler[E, R, A, B]): B ! R =
    Act(env => effects.handle(new okay.direct.TailHandler[Fam[E], A, B] {
      def value(a: A): B = exec(h.ret(a), env).asInstanceOf[B]
      def operation(op: Fam[E], k: Answer[Fam[E]]): Unit = op match {
        case o: Op[E, Any] @unchecked => k.answer(o, h.answer[Any](o.op))
      }
    }, body[E, A](p, env)))

  /** answered in place with a state: okay-direct's stateful handler, the state each resumption's own */
  override def handleLast[E[_], S, R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s0: S,
      h: StatefulTailHandler[E, S, R, A, B]): B ! R =
    Act(env => effects.handle(new okay.direct.StatefulHandler[Fam[E], S, A, B] {
      /** the state where the clause is: one cell for the handler, set from the state of each operation's time (a
       * clause reads and writes it while it runs, and keeps it no longer) */
      private[this] val cell = new Cell.Var[S](s0)
      def value(s: S, a: A): B = exec(h.ret(s, a), env).asInstanceOf[B]
      def operation(s: S, op: Fam[E], k: Answer[Fam[E]]): S = op match {
        case o: Op[E, Any] @unchecked =>
          cell.state = s
          k.answer(o, h.answer[Any](cell, o.op))
          cell.state
      }
    }, s0, body[E, A](p, env)))

  /** a general handler with a state: the state the body goes on with set at each resumption, before it */
  override def handleLast[E[_], S, R[_], A, B](p: A ! ({ type L[x] = Sum[R, E, x] })#L, s0: S,
      h: StatefulHandler[E, S, R, A, B]): B ! R =
    Act { env =>
      var state = s0
      general[E, A, B](p, env, a => exec(h.ret(state, a), env).asInstanceOf[B],
        (o, k) => exec(h.op[Any](state, o.op, (x: Any, next: S) => Act[B] { _ => state = next; k(x) }), env).asInstanceOf[B])
    }
}

object OnDirect {
  /** the capabilities of a row's effects, the last first */
  type Env = List[AnyRef]

  /** A PROGRAM: a trampoline, run with an environment */
  sealed abstract class Prog[+A]
  final case class Done[A](a: A) extends Prog[A]
  final case class Suspend[A](thunk: () => Prog[A]) extends Prog[A]
  final case class Bind[X, A](p: Prog[X], f: X => Prog[A]) extends Prog[A]
  /** a step done with the environment: a handler run */
  final case class Act[A](step: Env => A) extends Prog[A]
  /** an operation, performed through the capability at `offset` */
  final case class Perf[A](offset: Int, op: AnyRef) extends Prog[A]
  /** a program run in an environment changed: its effects moved */
  final case class Local[A](f: Env => Env, p: Prog[A]) extends Prog[A]

  /** the place of `E` in `R`: from the end, the last effect at 0 */
  class Place[E[_], R[_]](val offset: Int)
  final class Cut[E[_], R[_], Rest[_]](offset: Int) extends Place[E, R](offset)
  /** each effect of `R` found in `S`: an environment of `S` to one of `R` */
  final class Within[R[_], S[_]](val f: Env => Env)

  /** the family of okay-effects' effect `E` in okay-direct; its operations wrapped */
  sealed trait Fam[E[_]]
  final case class Op[E[_], X](op: E[X]) extends Fam[E] with okay.direct.Effect.Op[Fam[E], X]

  private def capability[E[_]](env: Env, offset: Int): okay.direct.Effect[Fam[E]] =
    env(offset).asInstanceOf[okay.direct.Effect[Fam[E]]]

  private def insert(env: Env, offset: Int, capability: AnyRef): Env = {
    val (before, after) = env.splitAt(offset)
    before ::: (capability :: after)
  }

  /** THE TRAMPOLINE'S STACK: immutable, so a continuation copied by a multi-shot implementation (Espresso's: the
   * Java stack copied, the heap shared) resumes each copy with its own frames. `f` a bind's function, or with `f`
   * null, `env` the environment to put back */
  private final class Frame(val f: Any => Prog[Any], val env: Env, val next: Frame)

  /** THE TRAMPOLINE: binds on a stack of its own, so in constant stack however deep; a value computed goes straight
   * to the frame on top — no `Done` made for an operation's answer, one frame a bind */
  def exec(program: Prog[Any], environment: Env): Any = {
    var p: Prog[Any] = program
    var env = environment
    var stack: Frame = null
    var value: Any = null
    var valued = false
    while (true) {
      if (valued) {
        if (stack == null) return value
        val top = stack
        stack = top.next
        if (top.f eq null) env = top.env
        else { valued = false; p = top.f(value) }
      } else p match {
        case Done(a) => value = a; valued = true
        case b: Bind[x, Any] @unchecked => stack = new Frame(b.f.asInstanceOf[Any => Prog[Any]], null, stack); p = b.p
        case Perf(offset, op) =>
          value = env(offset).asInstanceOf[okay.direct.Effect[AnyRef]].perform(op.asInstanceOf[okay.direct.Effect.Op[AnyRef, Any]])
          valued = true
        case Suspend(thunk) => p = thunk()
        case Act(step) => value = step(env); valued = true
        case Local(f, q) => stack = new Frame(null, env, stack); env = f(env); p = q
      }
    }
    throw new IllegalStateException("unreachable")
  }
}
