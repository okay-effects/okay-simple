package okay.effects

package object eff {
  /** the interface on THE `Eff` MONAD, with the standard effects, seen through the interface only:
   * `import okay.effects.eff.Eff.*` */
  val Eff: Std = new monad.OnEff with Std
}
