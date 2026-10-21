package okay.effects

import okay.effects.Runner

package object free {
  /** the interface on THE `Free` MONAD, seen as it is — its `inline` methods inlined where they are called:
   * `import okay.effects.free.OnFree.*`, its standard effects `import okay.effects.free.OnFree.std.*` */
  val OnFree: monad.OnFree = new monad.OnFree
  /** the same, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.free.Free.*`, its standard effects `import okay.effects.free.Free.std.*` */
  val Free: Runner = OnFree
  /** the free monad as the implicit implementation, opted into: `import okay.effects.free.implicits._` */
  object implicits {
    implicit val free: Free.type = Free
  }
}
