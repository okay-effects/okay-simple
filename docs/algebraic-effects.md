# Algebraic effects: what they are, and why

An **algebraic effect** is an interface of operations: what a program may ask for, without saying how it is done.
A **handler** is an implementation of that interface, chosen where the program is run. A program that performs an
operation stops there, and the handler decides what happens next: answer and go on, go on more than once, or never
go on.

This page explains:

- how effects differ from monads;
- why a handler is an exception handler that can go back;
- how both work, by delimited continuations.

It ends with a reading list; the foundation of what is implemented here is Kammar, Lindley and Oury's
[*Handlers in Action*](https://denotational.co.uk/publications/kammar-lindley-oury-handlers-in-action.pdf) (ICFP 2013).
Every `scala` block below is compiled and run, on the machine.

```scala
import okay.effects.*
import okay.effects.std.*
```

## An effect is an interface

An effect declares its operations, each with the type of its answer, and nothing else:

```scala
/** a field the program needs but may not have */
enum Problem[X]:
  case Missing(field: String) extends Problem[Int]

def missing[R[_]: Has[Problem]](field: String): Int ! R = perform[Problem](Problem.Missing(field))
```

A program uses the interface and says so in its type. `Int ! R`, where `R` has `Problem`, is a program that gives an
`Int` and may report a missing field:

```scala
def total[R[_]: Has[Problem]](form: Map[String, Int]): Int ! R =
  def field(name: String): Int ! R = form.get(name) match
    case Some(v) => pure(v)
    case None    => missing[R](name)
  for
    price <- field("price")
    tax   <- field("tax")
    tip   <- field("tip")
  yield price + tax + tip

val full = Map("price" -> 100, "tax" -> 20, "tip" -> 5)
val partial = Map("price" -> 100)
```

Nothing has happened yet: `total(partial)` is a value, a description. What a missing field MEANS is not decided here.

## One program, handled many ways

The same program, three handlers, three meanings.

**Like an exception.** The first problem stops the program. The handler never goes back, so the rest is dropped:

```scala
def failing[R[_], Rest[_], A](p: A ! R)(using Split[Problem, R, Rest]): Either[String, A] ! Rest =
  handle(p, new AbortHandler[Problem, Rest, A, Either[String, A]]:
    def ret(a: A) = pure(Right(a))
    def abort[X](op: Problem[X]) = op match
      case Problem.Missing(f) => pure(Left(s"missing $f")))

assert(run(failing(total[Problem](full))) == Right(125))
assert(run(failing(total[Problem](partial))) == Left("missing tax"))
```

**Resumed with a default.** The handler answers, and the program goes on from where it stopped, as if the field had
been there:

```scala
def defaulting[R[_], Rest[_], A](p: A ! R)(using Split[Problem, R, Rest]): A ! Rest =
  handle(p, new TailHandler[Problem, Rest, A, A]:
    def ret(a: A) = pure(a)
    def answer[X](op: Problem[X]): X = op match
      case Problem.Missing(_) => 0)

assert(run(defaulting(total[Problem](partial))) == 100)
```

**Resumed, and every problem reported.** The handler is given the rest of the program, `k`, as a function. It calls
`k` and adds what it saw to the result, so all the missing fields are collected in one run:

```scala
def reporting[R[_], Rest[_], A](p: A ! R)(using Split[Problem, R, Rest]): (List[String], A) ! Rest =
  handle(p, new Handler[Problem, Rest, A, (List[String], A)]:
    def ret(a: A) = pure((Nil, a))
    def op[X](op: Problem[X], k: X => (List[String], A) ! Rest) = op match
      case Problem.Missing(f) => k(0).map((seen, a) => (f :: seen, a)))

assert(run(reporting(total[Problem](partial))) == (List("tax", "tip"), 100))
```

A test, a production run and a validation report: one program, and the choice is made by the caller. The handler
can also call `k` more than once, for a search, every outcome, or a probability distribution. Two calls of `k` give
two futures of one past (the [tour](tour.md), step 4).

## Effects combine, predictably

A program states every effect it uses in its row, and the effects stay independent. Add a log beside the problem,
with no lifting, wrapping or new instances:

```scala
type Form = Problem + Writer % String

def logged(form: Map[String, Int]): Int ! Form =
  tell[String, Form]("start").andThen(total[Form](form)).flatMap(t => tell[String, Form](s"total $t").as(t))

assert(run(writer[String](defaulting(logged(partial)))) == (List("start", "total 100"), 100))
assert(run(writer[String](failing(logged(partial)))) == (List("start"), Left("missing tax")))
```

The order of the handlers is chosen when the program is run, not when it is written. `total` was written for any row
with `Problem`, and here it runs in one with a writer too. The meaning of each combination follows from the handlers:
the writer outside keeps the log written before the failure.

## Compared with monads

A monad says how to SEQUENCE computations: `pure` and `flatMap`. It does not say which operations a computation
may perform. Those come from somewhere else: a concrete type (`State[S, A]`, `Either[E, A]`), or a type class of
operations (`MonadState`, `MonadError`) with its instances.

Combining monads is where it gets hard:

- **Monad transformers** stack effects: `StateT[EitherT[IO, E, *], S, A]`. The order is fixed in the type. State
  over error and error over state are different programs, and the choice is made when the program is WRITTEN.
  Operations of an inner layer are `lift`ed through the outer ones.
- **Type classes of operations** (mtl style) hide the stack, but need an instance for every pair of an operation
  class and a transformer (the n² instances problem). Two effects of one kind, two states say, do not fit well.
- **A new effect** means a new transformer, its instances, and its interplay with every other.

With algebraic effects:

- the interface of an effect is just its operations, an `enum`;
- a program over several effects is an ordinary program, whose type lists them (`Problem + Writer % String`);
- the handlers come later, in any order, each implementing one effect;
- a new effect is an `enum` and a handler, with nothing to write for the effects already there.

Monads are not gone. They are the semantics: a handled program IS a monadic computation, and `!` is a monad. What
changes is how programs are built: from operations, with the monad assembled by the handlers.

## Exceptions that can go back

An exception handler sees where a failure came from only as data: the stack is already unwound and the place of the
`throw` is gone. A handler of an algebraic effect gets the place too, as `k`, the rest of the program from the
`perform`:

| | an exception | an effect's operation |
|---|---|---|
| the program | `throw e` | `perform(op)` |
| the handler | `catch` | `handle` |
| what the handler gets | `e` | `op` and `k`, the rest of the program |
| what it can do | recover here, the rest lost | not resume (an exception), resume once (an answer), resume many times (a search) |
| where it is declared | a throws clause, if anywhere | the row: `Int ! Problem` |

So an exception is an effect whose handler does not resume (the first handler above). Many familiar things are
handlers that do resume: state, readers, generators, `async`/`await`, coroutines, backtracking, probabilistic choice,
and dependency injection with a handler per environment.

## How it works: delimited continuations

A **delimited continuation** is the rest of a program up to a boundary, as a function. Two operators make one:

- `reset { body }` sets a boundary;
- inside `body`, `shift(k => …)` takes the rest of the body up to the `reset` as `k`. The `reset`'s answer is then
  what the function given to `shift` returns.

```scala
type K = Control % (String ! Pure)

/** `k` is "greet with the name, up to the reset": called once */
val once: String ! Pure =
  reset[String, Pure](shift[String, String, Pure, Pure + K](k => k("Ann")).map(name => s"hello, $name"))
assert(run(once) == "hello, Ann")

/** and called twice: the rest of the program run for each name */
val twice: String ! Pure =
  reset[String, Pure](shift[String, String, Pure, Pure + K](k => k("Ann").flatMap(a => k("Bob").map(b => s"$a; $b")))
    .map(name => s"hello, $name"))
assert(run(twice) == "hello, Ann; hello, Bob")
```

An effect handler is exactly that pair. **`handle` is a `reset`**: it sets the boundary, holding the handler's
clauses. **`perform` is a `shift`**: it takes the rest of the program up to the nearest handler of its effect as `k`,
and gives the operation and `k` to that handler's clause:

```text
handle(h) { body }   ≈   reset { body }, with h's clauses at the boundary; its value through h.ret
perform(op)          ≈   shift(k => h.op(op, k))     — h the nearest handler of op's effect
k(x)                 ≈   the rest of body from perform, under h again (a deep handler)
```

A handler resuming `k` once is a function call that went around the program. Not resuming is an exception. Resuming
twice runs the rest twice.

The effect's type picks which `reset` a `shift` goes to: the nearest handler of THAT effect, skipping the others.
That is how several effects live in one program. Handlers can also be implemented with the multi-prompt `shift0` and
`$` of okay-core.

The implementations here make `k` a value: the machine keeps an explicit stack of handlers, and a `perform` gives the
handler the frames up to it (see [implementations](guide/implementations.md), and
[delimited control](guide/delimited-control.md) for `shift`/`reset` as an effect and for a `reset` whose answer type
changes).

## Reading

Foundations: monads and their combination.

- Eugenio Moggi. *Notions of computation and monads.* Information and Computation, 1991.
- Philip Wadler. *The essence of functional programming.* POPL 1992.
- Sheng Liang, Paul Hudak, Mark Jones. *Monad transformers and modular interpreters.* POPL 1995.
- Andrzej Filinski. *Representing monads.* POPL 1994.

Algebraic effects and their handlers.

- Gordon Plotkin, John Power. *Adequacy for algebraic effects.* FoSSaCS 2001.
- Gordon Plotkin, John Power. *Notions of computation determine monads.* FoSSaCS 2002.
- Gordon Plotkin, John Power. *Algebraic operations and generic effects.* Applied Categorical Structures, 2003.
- Martin Hyland, Gordon Plotkin, John Power. *Combining effects: sum and tensor.* Theoretical Computer Science, 2006.
- Gordon Plotkin, Matija Pretnar. *Handlers of algebraic effects.* ESOP 2009.
- Matija Pretnar. *The logic and handling of algebraic effects.* PhD thesis, University of Edinburgh, 2010.
- Gordon Plotkin, Matija Pretnar. *Handling algebraic effects.* Logical Methods in Computer Science, 2013.
- Andrej Bauer, Matija Pretnar. *An effect system for algebraic effects and handlers.* CALCO 2013; LMCS 2014.
- Andrej Bauer, Matija Pretnar. *Programming with algebraic effects and handlers.* Journal of Logical and Algebraic
  Methods in Programming, 2015 (the language Eff).
- Matija Pretnar. *An introduction to algebraic effects and handlers.* MFPS 2015, invited tutorial: the best first
  paper to read.
- Andrej Bauer. *What is algebraic about algebraic effects and handlers?* Lecture notes, arXiv 2018.

Effects in languages and libraries.

- Ohad Kammar, Sam Lindley, Nicolas Oury. [*Handlers in action.*](https://denotational.co.uk/publications/kammar-lindley-oury-handlers-in-action.pdf) ICFP 2013: effect handlers,
  their semantics and their implementation in several languages — the foundation of this library.
- Oleg Kiselyov, Amr Sabry, Cameron Swords. *Extensible effects: an alternative to monad transformers.* Haskell
  Symposium 2013.
- Oleg Kiselyov, Hiromi Ishii. *Freer monads, more extensible effects.* Haskell Symposium 2015.
- Nicolas Wu, Tom Schrijvers, Ralf Hinze. *Effect handlers in scope.* Haskell Symposium 2014.
- Edwin Brady. *Programming and reasoning with algebraic effects and dependent types.* ICFP 2013 (Idris).
- Daan Leijen. *Koka: programming with row polymorphic effect types.* MSFP 2014.
- Daan Leijen. *Type directed compilation of row-typed algebraic effects.* POPL 2017.
- Daan Leijen. *Structured asynchrony with algebraic effects.* TyDe 2017.
- Ningning Xie, Daan Leijen. *Generalized evidence passing for effect handlers.* ICFP 2021.
- Daniel Hillerström, Sam Lindley. *Liberating effects with rows and handlers.* TyDe 2016 (Links).
- Daniel Hillerström, Sam Lindley, Robert Atkey, K. C. Sivaramakrishnan. *Continuation passing style for effect
  handlers.* FSCD 2017.
- Daniel Hillerström. *Foundations for programming and implementing effect handlers.* PhD thesis, University of
  Edinburgh, 2021.
- Sam Lindley, Conor McBride, Craig McLaughlin. *Do be do be do.* POPL 2017 (Frank).
- Stephen Dolan, Leo White, K. C. Sivaramakrishnan, Jeremy Yallop, Anil Madhavapeddy. *Effective concurrency through
  algebraic effects.* OCaml Workshop 2015.
- K. C. Sivaramakrishnan, Stephen Dolan, Leo White, Tom Kelly, Sadiq Jaffer, Anil Madhavapeddy. *Retrofitting effect
  handlers onto OCaml.* PLDI 2021.
- Jonathan Immanuel Brachthäuser, Philipp Schuster. *Effekt: extensible algebraic effects in Scala.* Scala Symposium
  2017.
- Jonathan Immanuel Brachthäuser, Philipp Schuster, Klaus Ostermann. *Effects as capabilities: effect handlers and
  lightweight effect polymorphism.* OOPSLA 2020.
- Dariusz Biernacki, Maciej Piróg, Piotr Polesiuk, Filip Sieczkowski. *Handle with care: relational interpretation
  of algebraic effects and handlers.* POPL 2018.

Delimited continuations.

- Matthias Felleisen. *The theory and practice of first-class prompts.* POPL 1988.
- Olivier Danvy, Andrzej Filinski. *Abstracting control.* LISP and Functional Programming 1990 (`shift` and
  `reset`).
- R. Kent Dybvig, Simon Peyton Jones, Amr Sabry. *A monadic framework for delimited continuations.* Journal of
  Functional Programming, 2007.
- Marek Materzok, Dariusz Biernacki. *Subtyping delimited continuations.* ICFP 2011 (`shift0`, as okay-core has it).
- Oleg Kiselyov. *Delimited control in OCaml, abstractly and concretely.* Theoretical Computer Science, 2012.
- Yannick Forster, Ohad Kammar, Sam Lindley, Matija Pretnar. *On the expressive power of user-defined effects: effect
  handlers, monadic reflection, delimited control.* ICFP 2017; Journal of Functional Programming, 2019.

More: Jeremy Yallop's [effects bibliography](https://github.com/yallop/effects-bibliography), a long list of papers,
languages and libraries, kept up to date.
