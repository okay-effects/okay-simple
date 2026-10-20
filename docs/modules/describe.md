# describe — an effect as a service

`okay-effects-describe`, `okay.effects.describe` — `import okay.effects.describe.*`. An effect's operations DESCRIBED
as data: an `Op` is a name, the parameters' `Shape`, the result's `Shape`, whether it writes, and `make`, the operation
with the identity answer as `perform` makes it; a `Describe[F]` is the operations and `split`, an operation back into
its description, its arguments and how its answer is read. From the two every projection follows mechanically, and
the program does not know which:

- `Domains.serve(d)(name, args)`: a call SERVED — the parameters decoded, `make(p)` performed under the handler in the
  row, the result encoded; the problems when the parameters do not decode, or the name is no operation. A REST
  resource, an MCP tool, a command line are each this plus a wire ([okay-faces](https://github.com/okay-effects/okay-faces)
  has REST and MCP).
- `Domains.remote(d)(transport)(program)`: the effect performed ELSEWHERE — a handler that splits each operation,
  carries it by `transport` (a program of the row outside: a fetch, a loopback, a function in a test) and answers what
  comes back. A result that does not decode is a `RemoteFailure`, not `Problems`: the service and the program disagree,
  which no person can fix at a form.

Written by hand, one `Op` per operation and one `match` for `split`: an effect's cases twice, as shapes and as a
match, on both compilers. A description is a value, not a type class: one effect may have two — the whole for its
owner, a subset for a stranger.

```scala
import okay.effects.{Sum, Void}
import okay.effects.json.Json
import okay.effects.machine.Machine
import okay.effects.machine.Machine.*
import okay.effects.shape.Shape.{field, int, record, unit}
import okay.effects.describe.*

sealed trait Counter[X]
object Counter:
  final case class Get[X](answer: Int => X) extends Counter[X]
  final case class Add[X](n: Int, answer: Int => X) extends Counter[X]

val get = Op[Counter, Unit, Int]("get", unit, int, _ => Counter.Get(x => x), Some("the count"))
val add = Op[Counter, Int, Int]("add", record(field("n", int))(n => n)(n => n), int, n => Counter.Add(n, x => x), Some("count up by n"), writes = true)
val counter: Describe[Counter] = new Describe[Counter]("counter"):
  val operations = List(get, add)
  def split[X](op: Counter[X]): Call[X] = op match
    case Counter.Get(k) => call(get, (), k)
    case Counter.Add(n, k) => call(add, n, k)

val domains = new Domains[Machine.type]()(Machine)
type C[X] = Sum[Void, Counter, X]
def counting[R[_], Rest[_], A](initial: Int)(p: A ! R)(using Split[Counter, R, Rest]): (Int, A) ! Rest =
  handle[Counter, Int, R, Rest, A, (Int, A)](p, initial, new StatefulTailHandler[Counter, Int, Rest, A, (Int, A)]:
    def ret(s: Int, a: A) = pure((s, a))
    def answer[X](cell: Cell[Int], op: Counter[X]): X = op match
      case Counter.Get(k) => k(cell.get)
      case Counter.Add(n, k) => cell.set(cell.get + n); k(cell.get))

// served by name: the parameters decoded, performed under the handler, the result encoded
val (n, served) = Machine.run(counting[C, Void, Either[okay.effects.shape.Problems, Json]](1)(domains.serve[Counter, C](counter)("add", Json.obj("n" -> Json.num(2)))))
assert(n == 3 && served == Right(Json.num(3)))

// performed from afar: a transport carries each operation to a service — here the same `serve`, in a run of its own
var state = 10
val transport: (Op[Counter, ?, ?], Json) => Json ! Void = (o, args) =>
  val (s, r) = Machine.run(counting[C, Void, Either[okay.effects.shape.Problems, Json]](state)(domains.serve[Counter, C](counter)(o.name, args)))
  state = s
  pure[Json, Void](r.toOption.get)
val program: Int ! C = perform[Counter].apply[C, Int](Counter.Add(5, x => x)).flatMap(_ => perform[Counter].apply[C, Int](Counter.Get(x => x)))
assert(Machine.run(domains.remote[Counter](counter).apply[C, Void, Int](transport)(program)) == 15 && state == 15)
```

Found in okay-faces 2026-10-20 (its `Describe`, the domain projection) and moved here the same day: an effect as a
service is the interface's concern, not a face's. okay-store describes its `Store` with it; verity its `Files`.
