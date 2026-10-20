package okay.effects

/** `import okay.effects.shape._`: the module on the machine — `read(shape)` through the `Fields` effect, and its
 * handlers `fromJson`, `answering` */
package object shape extends okay.effects.shape.Reading[okay.effects.machine.Machine.type]()(okay.effects.machine.Machine) {
  /** what a decoder says when it fails: never empty when returned */
  type Problems = List[Problem]
}
