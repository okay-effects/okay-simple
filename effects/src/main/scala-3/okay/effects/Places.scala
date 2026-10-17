package okay.effects

/**
 * TYPED PLACES, for an implementation's evidence (Scala 3): the place of an effect in a row as a path, typed by the
 * row — `Last` the last effect of `R + E`, `Init` one further in, `Only` a row of one — so that an implementation
 * matching a place against its handlers learns, by Scala 3's GADT inference, that an operation is its handler's:
 * no cast. Of no implementation: the machine and the `Free` monad's interpreter both take their `Split` to be a
 * `Place`, their `Sub` a `Renaming`.
 *
 * The matches over places are `@unchecked`: the checker cannot rule out an effect that is itself a row — `E` an
 * `R + G`, `Only` then meeting a handler — as an effect is any constructor. No effect is a row: a place's kind is
 * told by the row it is at, and the cases matched are all a row of that shape can have.
 */
object Places:
  type +[R[_], E[_]] = [X] =>> Sum[R, E, X]

  /** the place of `E` in `R`, and `R` without it `Rest` */
  enum Place[E[_], R[_], Rest[_]]:
    /** the one effect of a row of one */
    case Only[E[_]]() extends Place[E, E, Void]
    /** the last effect of `R + E` */
    case Last[R[_], E[_]]() extends Place[E, R + E, R]
    /** one further in than the last, `G` */
    case Init[E[_], R[_], G[_], Rest[_]](s: Place[E, R, Rest]) extends Place[E, R + G, Rest + G]

  /** places renamed from `R1` to `R2`, typed: a function, not data */
  trait Renaming[R1[_], R2[_]]:
    def apply[E[_]](at: Place[E, R1, ?]): Place[E, R2, ?]

  /** the places of `R` renamed to `Rest + E`'s: `E` moved to the end */
  def toEnd[E[_], R[_], Rest[_]](at: Place[E, R, Rest]): Renaming[R, Rest + E] = (at: @unchecked) match
    case _: Place.Only[e] => onlyToEnd[E]
    case _: Place.Last[r, e] => same[R]
    case i: Place.Init[e, r, g, rest] => initToEnd[E, r, g, rest](i.s)

  def same[R[_]]: Renaming[R, R] = new Renaming[R, R]:
    def apply[E[_]](at: Place[E, R, ?]): Place[E, R, ?] = at

  private def onlyToEnd[E[_]]: Renaming[E, Void + E] = new Renaming[E, Void + E]:
    private val last = Place.Last[Void, E]()
    def apply[E2[_]](at: Place[E2, E, ?]): Place[E2, Void + E, ?] = (at: @unchecked) match
      case _: Place.Only[e] => last

  private def initToEnd[E[_], R[_], G[_], Rest[_]](s: Place[E, R, Rest]): Renaming[R + G, Rest + G + E] =
    new Renaming[R + G, Rest + G + E]:
      // the places it renames to that are the same each time, made once
      private val gNow = Place.Init[G, Rest + G, E, Rest](Place.Last[Rest, G]())
      private val eNow = Place.Last[Rest + G, E]()
      private val inner = toEnd(s)
      def apply[E2[_]](at: Place[E2, R + G, ?]): Place[E2, Rest + G + E, ?] = (at: @unchecked) match
        case _: Place.Last[r, g] => gNow
        case ip: Place.Init[e2, r, g, rest] => (inner(ip.s): @unchecked) match
          case _: Place.Last[r3, e3] => eNow
          case ip2: Place.Init[e4, r4, g4, rest4] => Place.Init(Place.Init[E2, Rest, G, rest4](ip2.s))

  /** `Sub`'s three steps: no place in `Void`; a row of one, its place in `S`; `R + E`, `R`'s and `E`'s */
  def subPure[S[_]]: Renaming[Void, S] = new Renaming[Void, S]:
    def apply[E[_]](at: Place[E, Void, ?]): Place[E, S, ?] = throw IllegalStateException("no place in Pure")
  def subOne[E[_], S[_]](e: Place[E, S, ?]): Renaming[E, S] = new Renaming[E, S]:
    def apply[E2[_]](at: Place[E2, E, ?]): Place[E2, S, ?] = (at: @unchecked) match
      case _: Place.Only[e] => e
  def subMore[R[_], E[_], S[_]](r: Renaming[R, S], e: Place[E, S, ?]): Renaming[R + E, S] = new Renaming[R + E, S]:
    def apply[E2[_]](at: Place[E2, R + E, ?]): Place[E2, S, ?] = (at: @unchecked) match
      case _: Place.Last[r1, e1] => e
      case ip: Place.Init[e2, r2, g, rest] => r(ip.s)
