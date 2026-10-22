package okay.effects

/** `import okay.effects.parsing._`: the module on the machine, its words at once, nothing else to do */
package object parsing
  extends okay.effects.parsing.Parsing[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
  /** the input from a stream of chunks, on the default streams */
  lazy val fromStreams: okay.effects.parsing.StreamInput[okay.effects.stream.`package`.type] =
    new okay.effects.parsing.StreamInput[okay.effects.stream.`package`.type](okay.effects.stream.`package`)
  def inputFrom[Rest[_]](chunks: okay.effects.stream.Stream[String, Rest]): fromStreams.FromStream[Rest] =
    fromStreams.inputFrom[Rest](chunks)
  def parseStream[Rest[_]](chunks: okay.effects.stream.Stream[String, Rest]): fromStreams.ParseStream[Rest] =
    fromStreams.parseStream[Rest](chunks)
}
