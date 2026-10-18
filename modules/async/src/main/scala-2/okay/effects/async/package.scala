package okay.effects

/** `import okay.effects.async._`: the module on the machine, its words at once, nothing else to do */
package object async extends okay.effects.async.Asynchrony[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine, okay.effects.async.Timers.executionContext) {
  private lazy val merging = new okay.effects.async.AsyncStreams[okay.effects.machine.Machine.type, okay.effects.stream.`package`.type](okay.effects.stream.`package`)(
    okay.effects.async.Timers.executionContext)
  /** streams of `Async` merged as their elements come: `AsyncStreams`'s, on the default streams */
  def mergeAsync[O](ss: List[okay.effects.stream.Stream[O, Async]]): okay.effects.stream.Stream[O, Async] = merging.mergeAsync[O](ss)
}
