package okay.effects

import okay.effects.Std

package object core {
  /** okay-effects' interface on OKAY-CORE'S MACHINE, with the standard effects, seen through the interface only:
   * `import okay.effects.core.Core._` */
  val Core: Std = new OnCore with Std
}
