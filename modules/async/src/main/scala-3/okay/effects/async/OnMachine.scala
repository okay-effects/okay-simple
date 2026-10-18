package okay.effects.async

/** `import okay.effects.async.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Asynchrony()(using okay.effects.machine.Machine, okay.effects.async.Timers.executionContext)
export onMachine.*
private lazy val merging = new AsyncStreams[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](okay.effects.stream.onMachine)(using
  okay.effects.async.Timers.executionContext)
/** streams of `Async` merged as their elements come: `AsyncStreams`'s, on the default streams */
def mergeAsync[O](ss: List[okay.effects.stream.Stream[O, Async]]): okay.effects.stream.Stream[O, Async] = merging.mergeAsync[O](ss)
