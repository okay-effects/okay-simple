package okay.effects.simple

import okay.effects.Std

package object free {
  /** the interface on THE `Free` MONAD, with the standard effects, seen through the interface only:
   * `import okay.effects.simple.free.Free.*` */
  val Free: Std = new monad.OnFree with Std
}
