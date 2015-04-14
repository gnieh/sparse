/* Copyright (c) 2015 Lucas Satabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package slang
package sparse
package generator

import untyped._
import util._

sealed trait Lookahead[S]

final case class Spontaneous[S](t: Token[S]) extends Lookahead[S]

final case class Propagated[S](id: StateI, item: SlrItem[S]) extends Lookahead[S]

object Lookahead {

  def compute[S](istate: StateI, kernels: Set[SlrItem[Option[S]]], sym: Symbol[Option[S]]): Generator[SlrItem, Option[S], LookaheadTable[S]] = {
    def computeItem(jstate: StateI, a: SlrItem[Option[S]]): Set[((StateI, SlrItem[Option[S]]), Lookahead[Option[S]])] =
      for {
        b <- LalrItem.closure(Set(a.toLalr(Tok(None))))
        if b.nextSymbol == Tok(sym)
      } yield if(b.lookahead != Tok(None)) {
        ((jstate, b.toSlr.next), Spontaneous(b.lookahead))
      } else {
        ((jstate, b.toSlr.next), Propagated(istate, a))
      }
    for {
      st <- askGoto(istate, sym)
      table <- st.fold(Reader.unit[GeneratorEnv[SlrItem, Option[S]], LookaheadTable[S]](Map())) { jstate =>
          for {
            startState <- Reader.ask((env: GeneratorEnv[SlrItem, Option[S]]) => env.startState)
            startRid <- Reader.ask((env: GeneratorEnv[SlrItem, Option[S]]) => env.startRule)
            startItem = SlrItem.startItem(startRid)
          } yield (for {
            k <- kernels
            las <- computeItem(jstate, k)
          } yield las).toMultiMap.addBinding((startState, startItem), Spontaneous(EOI))
        }
    } yield table
  }

  def find[S](table: LookaheadTable[S], state: StateI, item: SlrItem[Option[S]]): Done[(Int, SlrItem[Option[S]]), Unit, Set[Token[Option[S]]]] = {
    def go(lookahead: Lookahead[Option[S]]): Done[(Int, SlrItem[Option[S]]), Unit, Set[Token[Option[S]]]] = lookahead match {
      case Spontaneous(t)       => State.unit(Set(t))
      case Propagated(st, item) => find(table, st, item)
    }
    val lookaheads = table(state -> item)
    val done =
      for {
        () <- Done.put(state -> item, ())
        d <- State.mapM(go)(lookaheads.toList)
      } yield d.flatten.toSet
    done.ifNotDoneG(state -> item, _ => Set.empty)
  }

}
