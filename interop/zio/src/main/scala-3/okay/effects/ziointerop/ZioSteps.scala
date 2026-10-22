package okay.effects.ziointerop

import okay.effects.{Clauses, Runner, Sum}
import zio.{ZEnvironment, ZIO}

/** a ZIO of environment `Env` and error `Err` as an effect of its own: a `ZIO[Env, Err, X]` its operation */
type ZioStep[Env, Err] = [X] =>> ZIO[Env, Err, X]

/**
 * ZIO'S OWN TYPES AS EFFECTS, as okay has them (Scala 3): each `ZIO[Env, Err, *]` an effect of the row, a ZIO kept as
 * one operation and run when the program is run — so a program holds ZIOs of DIFFERENT environments and errors, one
 * member each: `Int ! (ZioStep[Db, DbErr] + ZioStep[Log, LogErr])`.
 *
 *  - `z.step`: the ZIO performed, an operation of `ZioStep[Env, Err]`;
 *  - `p.toZIOSteps`: the program as ONE ZIO, of the type ZIO's own `for` gives — the members' environments met
 *    (`Db & Log`), their errors joined (`DbErr | LogErr`, an `AppErr` where that is their parent), read off the row by
 *    `Members`;
 *  - `p.provideSteps(env)`, `p.mapErrorSteps(f)`: the environment given, the error mapped, as ZIO's
 *    `provideEnvironment` and `mapError` — the program then a ZIO of one member.
 *
 * A row of ZIO members only: another effect is handled before. The typed way to mix ZIO with the other effects is
 * `ZioRow` (`fromZIOTyped`), its channels our `Reader`, `Error`, `Async`.
 */
class ZioSteps[F <: Runner with Singleton](using implementation: F):
  val fx: F = implementation
  import fx.{!, In, Pure}

  extension [Env, Err, A](z: ZIO[Env, Err, A])
    /** the ZIO an operation of its own effect, `ZioStep[Env, Err]` */
    def step[R[_]](using in: In[ZioStep[Env, Err], R]): A ! R = fx.perform[ZioStep[Env, Err]][R, A](z)

  /**
   * A ROW OF ZIO MEMBERS ONLY, and what its steps need and raise: `Env` the members' environments met, `Err` their
   * errors joined; `run` the program as one ZIO — each operation the ZIO it is, flatMapped into the rest.
   */
  trait Members[R[_]]:
    type Env
    type Err
    def run[A](p: A ! R): ZIO[Env, Err, A]

  object Members:
    given one[Env0, Err0]: (Members[ZioStep[Env0, Err0]] { type Env = Env0; type Err = Err0 }) =
      new Members[ZioStep[Env0, Err0]]:
        type Env = Env0
        type Err = Err0
        def run[A](p: A ! ZioStep[Env0, Err0]): ZIO[Env0, Err0, A] =
          fx.run(fx.handle[ZioStep[Env0, Err0], ZioStep[Env0, Err0], Pure, A, ZIO[Env0, Err0, A]](p,
              new Clauses[ZioStep[Env0, Err0], A, ZIO[Env0, Err0, A] ! Pure]:
                def ret(a: A) = fx.pure[ZIO[Env0, Err0, A], Pure](ZIO.succeed(a))
                def op[X](z: ZIO[Env0, Err0, X], k: X => ZIO[Env0, Err0, A] ! Pure) =
                  fx.pure[ZIO[Env0, Err0, A], Pure](z.flatMap(x => fx.run(k(x))))
            )(using fx.splitOne[ZioStep[Env0, Err0]]))

    given more[G[_], Env1, Err1](using g: Members[G]): (Members[[X] =>> Sum[G, ZioStep[Env1, Err1], X]] {
      type Env = g.Env & Env1; type Err = g.Err | Err1
    }) =
      new Members[[X] =>> Sum[G, ZioStep[Env1, Err1], X]]:
        type Env = g.Env & Env1
        type Err = g.Err | Err1
        def run[A](p: A ! ([X] =>> Sum[G, ZioStep[Env1, Err1], X])): ZIO[Env, Err, A] =
          g.run(fx.handleLast[ZioStep[Env1, Err1], G, A, ZIO[Env, Err, A]](p,
              new Clauses[ZioStep[Env1, Err1], A, ZIO[Env, Err, A] ! G]:
                def ret(a: A) = fx.pure[ZIO[Env, Err, A], G](ZIO.succeed(a))
                def op[X](z: ZIO[Env1, Err1, X], k: X => ZIO[Env, Err, A] ! G) =
                  fx.pure[ZIO[Env, Err, A], G](z.flatMap(x => g.run(k(x)).flatten))
            )).flatten

  extension [A, R[_]](p: A ! R)
    /** the program's ZIO steps as ONE ZIO, of the members' environments met and errors joined */
    def toZIOSteps(using m: Members[R]): ZIO[m.Env, m.Err, A] = ZIO.suspendSucceed(m.run(p))
    /** the environment given to every step: a program of one ZIO member, of no environment */
    def provideSteps(using m: Members[R])(env: ZEnvironment[m.Env]): A ! ZioStep[Any, m.Err] =
      p.toZIOSteps.provideEnvironment(env).step[ZioStep[Any, m.Err]](using fx.splitOne)
    /** every step's typed failure mapped, as ZIO's `mapError` */
    def mapErrorSteps[Err2](using m: Members[R])(f: m.Err => Err2): A ! ZioStep[m.Env, Err2] =
      p.toZIOSteps.mapError(f).step[ZioStep[m.Env, Err2]](using fx.splitOne)

object ZioSteps:
  def apply()(using fx: Runner): ZioSteps[fx.type] =
    given exact: fx.type = fx
    new ZioSteps[fx.type]
