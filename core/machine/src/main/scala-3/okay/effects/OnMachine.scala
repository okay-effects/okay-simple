package okay.effects

import okay.effects.machine.Machine

/**
 * THE INTERFACE ON THE MACHINE, BY DEFAULT: `import okay.effects.*` gives a program's types and words — `!`, the rows'
 * `+` (`%` and `*` are the package's own), `Has`, `perform`, `handle` of each shape, `intercept`, `run` — on the
 * machine; `import okay.effects.std.*` its standard effects; `import okay.effects.delimited.*` its delimited control
 * changing answer types. Each module's package does the same for the module: `import okay.effects.stream.*`.
 */
export Machine.{!, Pure, In, Split, Sub, Has, +, Handler, TailHandler, AbortHandler, StatefulHandler, StatefulTailHandler,
  splitOne, splitLast, splitInit, subPure, subOne, subMore,
  pure, flatMap, map, as, andThen, into, delay, tailcall, tailRecM, perform, split, embed, handle, handleLast, intercept,
  run, lower, std, delimited}
