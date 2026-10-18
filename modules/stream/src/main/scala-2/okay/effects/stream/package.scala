package okay.effects

/** `import okay.effects.stream._`: the module on the machine, its words at once, nothing else to do */
package object stream extends okay.effects.stream.Streams[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
  private lazy val running = new okay.effects.stream.StreamsRun[okay.effects.machine.Machine.type, this.type](this)
  /** a stream of no effect, run lazily on the machine: a lazy list, an iterator */
  def toLazyList[O](s: Stream[O, okay.effects.Void]): LazyList[O] = running.toLazyList[O](s)
  def toIterator[O](s: Stream[O, okay.effects.Void]): Iterator[O] = running.toIterator[O](s)
}
