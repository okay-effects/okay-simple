package okay.api

import okay.std.Std

package object core {
  /** okay-api's interface on OKAY-CORE'S MACHINE, with the standard effects, seen through the interface only:
   * `import okay.api.core.Core._` */
  val Core: Std = new OnCore with Std
}
