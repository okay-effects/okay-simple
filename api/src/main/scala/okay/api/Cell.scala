package okay.api

/** a stateful handler's state where an operation is: read and written by its clause, in place — of no
 * implementation's, so that one's static code takes it as it is */
trait Cell[S] { def get: S; def set(s: S): Unit }
