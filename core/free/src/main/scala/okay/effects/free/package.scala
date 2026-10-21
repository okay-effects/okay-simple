package okay.effects

import okay.effects.{Runner, Self}

package object free {
  /** the interface on THE `Free` MONAD, seen as it is — its `inline` methods inlined where they are called:
   * `import okay.effects.free.OnFree.*`, its standard effects `import okay.effects.free.OnFree.std.*` */
  val OnFree: monad.OnFree with Self = new monad.OnFree with Self
  /** the same, seen through the interface only — its own types abstract to its users:
   * `import okay.effects.free.Free.*`, its standard effects `import okay.effects.free.Free.std.*` */
  val Free: Runner with Self = OnFree
}
