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

import scala.language.higherKinds

final case class GeneratorEnv[I[T] <: Item[I, T], S](itemSets: List[(Set[I[S]], StateI)],
  itemSetIndex: Map[Set[I[S]], StateI],
  rules: List[RuleId[S]],
  terminals: List[Symbol[S]],
  nonTerminals: List[Symbol[S]],
  symbols: List[Symbol[S]],
  startState: StateI,
  startRule: RuleId[S],
  gotos: Map[(StateI, Symbol[S]), StateI])

object GeneratorEnv {

  def create[I[T] <: Item[I, T]: MetaItem, S](rid: RuleId[S]): GeneratorEnv[I, S] = {
    val rules = rid.rules
    val terms = terminals(rules)
    val nonTerms = nonTerminals(rules)
    val syms = terms ++ nonTerms
    val items = itemSets[I, S](rid, syms.toSet).toList.zipWithIndex
    val itemIdx = items.toMap
    GeneratorEnv(items,
      itemIdx,
      rules,
      terms,
      nonTerms,
      syms,
      items.find(_._1.contains(implicitly[MetaItem[I]].startItem(rid))).get._2,
      rid,
      precomputeGotos(items, itemIdx, syms))
  }

}
