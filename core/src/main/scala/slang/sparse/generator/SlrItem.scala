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

final case class SlrItem[S](id: RuleId[S], prod: Int, pos: Int) extends Item[SlrItem, S] {

  def next: SlrItem[S] =
    copy(pos = pos + 1)

  def toLalr(lookahead: Token[S]): LalrItem[S] =
    LalrItem(id, prod, pos, lookahead)

}

object SlrItem extends MetaItem[SlrItem] {

  def startItem[S](r: RuleId[S]): SlrItem[S] =
    SlrItem(r, 0, 0)

  def closure[S](set: Set[SlrItem[S]]): Set[SlrItem[S]] =
    set.recTraverseG(closure1)

  private def closure1[S](items: Set[SlrItem[S]]): (Set[SlrItem[S]], Set[SlrItem[S]]) = {
    val res = items.map(closureItem).flatten
    (items ++ res, res)
  }

  private def closureItem[S](item: SlrItem[S]): Set[SlrItem[S]] = item.nextSymbol match {
    case Tok(RuleSymbol(rid)) =>
      (for(p <- 0 until item.id.rule.size)
        yield SlrItem(rid, p, 0)).toSet
    case _ =>
      Set.empty
  }

  def toLalrGenerator[S](slrGen: GeneratorEnv[SlrItem, Option[S]]): GeneratorEnv[LalrItem, Option[S]] =
    ???

}
