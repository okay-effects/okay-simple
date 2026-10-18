package okay.effects

/** `import okay.effects.kyointerop.*`: kyo on the machine, over the default streams, its words at once — a package
 * object, not top-level exports: an export of the class it comes from would have the compiler type it while typing it */
package object kyointerop extends okay.effects.kyointerop.KyoEffects[okay.effects.machine.Machine.type, okay.effects.stream.onMachine.type](
  okay.effects.stream.onMachine)
