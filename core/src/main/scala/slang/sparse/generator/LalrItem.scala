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

import util._
import untyped._

final case class LalrItem[S](id: RuleId[S], prod: Int, pos: Int, lookahead: Token[S]) extends Item[LalrItem, S] {

  def next: LalrItem[S] =
    copy(pos = pos + 1)

  def toSlr: SlrItem[S] =
    SlrItem(id, prod, pos)

}

object LalrItem extends MetaItem[LalrItem] {

  def startItem[S](r: RuleId[S]): LalrItem[S] =
    LalrItem(r, 0, 0, EOI)

  def closure[S](set: Set[LalrItem[S]]): Set[LalrItem[S]] =
    set.recTraverseG(closure1)

  private def closure1[S](items: Set[LalrItem[S]]): (Set[LalrItem[S]], Set[LalrItem[S]]) = {
    val res = items.map(closureItem).flatten
    (items ++ res, res)
  }

  private def closureItem[S](item: LalrItem[S]): Set[LalrItem[S]] = item.nextSymbol match {
    case Tok(RuleSymbol(rid)) =>
      // drop the firs `item.pos + 1` symbol of the productiion
      val dropped = item.id.rule(item.prod).drop(item.pos + 1)
      for {
        b <- firstA(dropped, item.lookahead)
        it <- firsts(rid, b)
      } yield it
    case _ =>
      Set.empty
  }

  private def firstA[S](prod: Production[S], sym: Token[S]): Set[Token[S]] = {
    val firstsProd = prod.firsts
    firstsProd.map {
      case ETok(t) => Tok(t)
      case Epsilon => sym
    }
  }

  private def firsts[S](r: RuleId[S], lookahead: Token[S]): Set[LalrItem[S]] =
    (for(p <- 0 until r.rule.size)
      yield LalrItem(r, p, 0, lookahead)).toSet

}
