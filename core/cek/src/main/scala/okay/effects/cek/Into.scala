package okay.effects.cek

import okay.effects.{Sum, Void}
import okay.scala3.{At, Sub as Renaming}

/**
 * AN OKAY-SIMPLE PROGRAM IN AN OKAY-SCALA ROW: the program is okay-scala's already (on `OnCek` its `!` is `Prog`);
 * only its row is this interface's (`Sum`, `Void`, the places `Spot`). `Into[R, S]` renames `R`'s places to `S`'s —
 * each of `R`'s effects found in `S` by okay-scala's own `In` — so the program is one of `S`, its operations handled
 * by okay-scala's handlers among the rest of an okay-scala program: one machine, one run, nothing wrapped. Its own
 * handlers stay inside it, as written (okay-store's translations into `Data`, okay-faces' faces).
 *
 * `OnCek.into[S](p)`. The rows written as this interface writes them: `Void` first, an effect added by `Sum`.
 */
trait Into[R[_], S[_]]:
  def renaming: Renaming[R, S]

object Into:
  private type +[R[_], E[_]] = [X] =>> Sum[R, E, X]

  /** no effect: `Void`'s places — this interface's empty effect, never an operation's — to its empty place in `S` */
  given pure[S[_]]: Into[Void, S] = new Into[Void, S]:
    val renaming: Renaming[Void, S] = new Renaming[Void, S]:
      def apply[E[_]](at: At[E, Void, ?]): At[E, S, ?] = (at: @unchecked) match
        case _: OnCek.Spot.Only[e] => OnCek.Spot.Absurd[S]()
        case _: OnCek.Spot.Absurd[r] => OnCek.Spot.Absurd[S]()

  /** `R + E`: `E`'s place in `S`, and `R`'s by `R`'s own */
  given more[R[_], E[_], S[_]](using r: Into[R, S], e: okay.scala3.In[E, S]): Into[R + E, S] = new Into[R + E, S]:
    val renaming: Renaming[R + E, S] = new Renaming[R + E, S]:
      def apply[E2[_]](at: At[E2, R + E, ?]): At[E2, S, ?] = (at: @unchecked) match
        case _: OnCek.Spot.Last[r, e] => e.place
        case i: OnCek.Spot.Init[e2, r2, g, rest] => r.renaming(i.at)
        case _: OnCek.Spot.Absurd[r] => OnCek.Spot.Absurd[S]()
