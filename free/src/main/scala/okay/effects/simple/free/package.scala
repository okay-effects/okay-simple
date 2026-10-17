package okay.effects.simple

import okay.effects.Effects

package object free {
  /** the interface on THE `Free` MONAD, seen as it is — its `inline` methods inlined where they are called:
   * `import okay.effects.simple.free.OnFree.*`, its standard effects `import okay.effects.simple.free.OnFree.std.*` */
  val OnFree: monad.OnFree = new monad.OnFree
  /** the same, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.simple.free.Free.*`, its standard effects `import okay.effects.simple.free.Free.std.*` */
  val Free: Effects = OnFree
}
