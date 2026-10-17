package okay.effects

package object core {
  /** okay-effects' interface on OKAY-CORE'S MACHINE, seen through the interface only: `import okay.effects.core.Core._`,
   * and its standard effects `import okay.effects.core.Core.std._` */
  val Core: Effects = new OnCore
}
