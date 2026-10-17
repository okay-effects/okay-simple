package okay.effects.examples

import okay.effects.Effects
import okay.effects.simple.Machine
import okay.effects.simple.free.{Free, OnFree}

/**
 * ONE PROGRAM, ANY IMPLEMENTATION: written over `Effects`, run on the machine and on the `Free` monad — the same
 * value. An implementation offered as `Effects` keeps its types to itself; `OnFree` is the `Free` monad seen as it
 * is, its `inline` methods inlined where they are called.
 */
class Greeting(val fx: Effects):
  import fx.*
  import fx.std.*

  def greet[R[_]: {Has[Reader % String], Has[State % Int]}]: String ! R =
    for name <- ask[String, R]; n <- get[Int, R]; _ <- put[Int, R](n + name.length) yield s"$name #$n"

  def greeted(name: String): (Int, String) = run(state(1)(reader(name)(greet[Reader % String + State % Int])))

object Implementations3:
  def onMachine: (Int, String) = Greeting(Machine).greeted("Ann")
  def onFree: (Int, String) = Greeting(Free).greeted("Ann")

  /** the `Free` monad seen as it is: its programs `Free` trees, built by its own `inline` primitives */
  def onFreeAsItIs: Int =
    import OnFree.*
    import OnFree.std.*
    run(state(20)(modify[Int, State % Int](_ * 2).andThen(get[Int, State % Int])))._2
