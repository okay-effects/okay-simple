package okay.api

/** the operation of the effect `E` answered by an `X`, of no implementation's: `Effects.Op`, here for an
 * implementation's static code */
type OpOf[E <: Effect, X] = E match { case Name[f] => f[X] }
