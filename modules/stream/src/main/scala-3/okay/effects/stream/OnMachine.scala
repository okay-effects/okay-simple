package okay.effects.stream

/** `import okay.effects.stream.*`: the module on the machine, its words at once, nothing else to do */
val onMachine = Streams()(using okay.effects.machine.Machine)
export onMachine.*
private lazy val running: StreamsRun[okay.effects.machine.Machine.type, onMachine.type] = new StreamsRun(onMachine)
/** a stream of no effect, run lazily on the machine: a lazy list, an iterator */
def toLazyList[O](s: Stream[O, okay.effects.Void]): LazyList[O] = running.toLazyList[O](s)
def toIterator[O](s: Stream[O, okay.effects.Void]): Iterator[O] = running.toIterator[O](s)
