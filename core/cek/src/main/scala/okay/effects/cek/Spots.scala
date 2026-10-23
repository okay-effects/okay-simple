package okay.effects.cek

import okay.effects.{Sum, Void}
import okay.effects.cek.OnCek.Spot
import okay.scala3.{At, Escape, Sub as Renaming, Taken}

/** the adapter's places compared, renamed, refuted at the top: okay-scala's own, for this interface's rows */
private[cek] object Spots:
  type +[R[_], E[_]] = [X] =>> Sum[R, E, X]

  /** an operation at `at` met by the handler at `mine`, both in one row */
  def take[E[_], R[_], Rest[_], E2[_]](mine: Spot[E, R, Rest], at: At[E2, R, ?]): Taken[E, E2, Rest] =
    (mine: @unchecked) match
      case l: Spot.Last[r, e] => (at: @unchecked) match
          case _: Spot.Last[r2, e2] => proof(l)
          case i: Spot.Init[e2, r2, g, rest] => Taken.Theirs(i.at)
          case _: Spot.Absurd[r2] => Taken.Theirs(Spot.Absurd())
      case o: Spot.Only[e] => (at: @unchecked) match
          case _: Spot.Only[e2] => proof(o)
          case _: Spot.Absurd[r2] => Taken.Theirs(Spot.Absurd())
      case i: Spot.Init[e, r, g, rest] => (at: @unchecked) match
          case _: Spot.Last[r2, e2] => Taken.Theirs(Spot.Last())
          case j: Spot.Init[e2, r2, g2, rest2] => take(i.at, j.at) match
              case _: Taken.Mine[e3, r3] => Taken.Mine()
              case t: Taken.Theirs[e3, e4, r4] => (t.at: @unchecked) match
                  case s: Spot[e4, r4, ?] @unchecked => Taken.Theirs(Spot.Init(s))
          case _: Spot.Absurd[r2] => Taken.Theirs(Spot.Absurd())
      case _: Spot.Absurd[r] => Taken.Theirs(at)

  private def proof[R[_], E[_]](l: Spot.Last[R, E]): Taken[E, E, R] = l.proof match
    case null =>
      val made = Taken.Mine[E, R]()
      l.proof = made
      made
    case made: Taken[E, E, R] @unchecked => made

  private def proof[E[_]](o: Spot.Only[E]): Taken[E, E, Void] = o.proof match
    case null =>
      val made = Taken.Mine[E, Void]()
      o.proof = made
      made
    case made: Taken[E, E, Void] @unchecked => made

  /** the operation's place outside the handler at `mine`, or `null` if it is the handler's: nothing made on the way */
  def outside[E[_], R[_], Rest[_], E2[_]](mine: Spot[E, R, Rest], at: At[E2, R, ?]): At[E2, Rest, ?] | Null =
    (mine: @unchecked) match
      case _: Spot.Last[r, e] => (at: @unchecked) match
          case _: Spot.Last[r2, e2] => null
          case i: Spot.Init[e2, r2, g, rest] => i.at
          case _: Spot.Absurd[r2] => Spot.Absurd()
      case _: Spot.Only[e] => (at: @unchecked) match
          case _: Spot.Only[e2] => null
          case _: Spot.Absurd[r2] => Spot.Absurd()
      case i: Spot.Init[e, r, g, rest] => (at: @unchecked) match
          case _: Spot.Last[r2, e2] => Spot.Last()
          case j: Spot.Init[e2, r2, g2, rest2] => (outside(i.at, j.at): @unchecked) match
              case null => null
              case q: Spot[E2, rest, ?] @unchecked => Spot.Init(q)
          case _: Spot.Absurd[r2] => Spot.Absurd()
      case _: Spot.Absurd[r] => at

  /** past every handler an operation is at `Pure`'s one place or the empty effect's: of `Void`, absurd */
  val escape: Escape[Void] = new Escape[Void]:
    def apply[E[_], X](at: At[E, Void, ?], op: E[X]): Nothing = (at: @unchecked) match
      case _: Spot.Only[e] => op.absurd
      case _: Spot.Absurd[r] => op.absurd

  // -- RENAMINGS: `split`, `embed`, `intercept`

  def same[R[_]]: Renaming[R, R] = new Renaming[R, R]:
    def apply[E[_]](at: At[E, R, ?]): At[E, R, ?] = at

  /** `R`'s places renamed to `Rest + E`'s: `E` from `at` to the end */
  def toEnd[E[_], R[_], Rest[_]](at: Spot[E, R, Rest]): Renaming[R, Rest + E] = (at: @unchecked) match
    case _: Spot.Only[e] => onlyToEnd[E]
    case _: Spot.Last[r, e] => same[R]
    case i: Spot.Init[e, r, g, rest] => initToEnd[E, r, g, rest](i.at)
    case _: Spot.Absurd[r] => further[R]

  /** `R`'s places renamed to `R + E`'s: `E`'s at `at` to the new last place, the others where they are */
  def interposing[E[_], R[_]](at: Spot[E, R, ?]): Renaming[R, R + E] = new Renaming[R, R + E]:
    private val last = Spot.Last[R, E]()
    def apply[E2[_]](p: At[E2, R, ?]): At[E2, R + E, ?] = take(at, p) match
      case _: Taken.Mine[e, r] => last
      case _: Taken.Theirs[e, e2, r] => (p: @unchecked) match
          case _: Spot.Absurd[r2] => Spot.Absurd[R + E]()
          case q: Spot[E2, R, ?] @unchecked => Spot.Init(q)

  private def further[R[_]]: Renaming[R, R + Void] = new Renaming[R, R + Void]:
    def apply[E[_]](at: At[E, R, ?]): At[E, R + Void, ?] = (at: @unchecked) match
      case _: Spot.Absurd[r] => Spot.Absurd[R + Void]()
      case p: Spot[E, R, ?] @unchecked => Spot.Init(p)

  private def onlyToEnd[E[_]]: Renaming[E, Void + E] = new Renaming[E, Void + E]:
    private val last = Spot.Last[Void, E]()
    def apply[E2[_]](at: At[E2, E, ?]): At[E2, Void + E, ?] = (at: @unchecked) match
      case _: Spot.Only[e] => last
      case _: Spot.Absurd[r] => Spot.Absurd[Void + E]()

  private def initToEnd[E[_], R[_], G[_], Rest[_]](at: Spot[E, R, Rest]): Renaming[R + G, Rest + G + E] =
    new Renaming[R + G, Rest + G + E]:
      private val gNow = Spot.Init[G, Rest + G, E, Rest](Spot.Last[Rest, G]())
      private val eNow = Spot.Last[Rest + G, E]()
      private val inner = toEnd(at)
      def apply[E2[_]](p: At[E2, R + G, ?]): At[E2, Rest + G + E, ?] = (p: @unchecked) match
        case _: Spot.Last[r, g] => gNow
        case _: Spot.Absurd[r] => Spot.Absurd[Rest + G + E]()
        case i: Spot.Init[e2, r, g, rest] => (inner(i.at): @unchecked) match
            case _: Spot.Last[r3, e3] => eNow
            case j: Spot.Init[e4, r4, g4, rest4] => Spot.Init(Spot.Init[E2, Rest, G, rest4](j.at))
            case _: Spot.Absurd[r5] => Spot.Absurd[Rest + G + E]()

  // -- SUB: a smaller row's places in a bigger one

  def subPure[S[_]]: Renaming[Void, S] = new Renaming[Void, S]:
    def apply[E[_]](at: At[E, Void, ?]): At[E, S, ?] = (at: @unchecked) match
      case _: Spot.Only[e] => Spot.Absurd[S]()
      case _: Spot.Absurd[r] => Spot.Absurd[S]()

  def subOne[E[_], S[_]](e: Spot[E, S, ?]): Renaming[E, S] = new Renaming[E, S]:
    def apply[E2[_]](at: At[E2, E, ?]): At[E2, S, ?] = (at: @unchecked) match
      case _: Spot.Only[e] => e
      case _: Spot.Absurd[r] => Spot.Absurd[S]()

  def subMore[R[_], E[_], S[_]](r: Renaming[R, S], e: Spot[E, S, ?]): Renaming[R + E, S] = new Renaming[R + E, S]:
    def apply[E2[_]](at: At[E2, R + E, ?]): At[E2, S, ?] = (at: @unchecked) match
      case _: Spot.Last[r1, e1] => e
      case i: Spot.Init[e2, r2, g, rest] => r(i.at)
      case _: Spot.Absurd[r] => Spot.Absurd[S]()
