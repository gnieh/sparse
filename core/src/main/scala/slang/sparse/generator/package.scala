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

import util._
import untyped._

import scala.language.higherKinds

package object generator {

  type StateI = Int
  type StackI = Int

  type ActionTable[S] = List[(StateI, (List[(Token[S], Action[S])], Action[S]))]
  type GotoTable[S] = List[((StateI, RuleI), StateI)]

  type ActionFun[S] = (StateI, Token[S]) => Action[S]
  type GotoFun[S] = (StateI, RuleI) => StateI

  type LookaheadTable[S] = MultiMap[(Int, SlrItem[Option[S]]), Lookahead[Option[S]]]

  def LookaheadTable[S]: LookaheadTable[S] =
    Map[(Int, SlrItem[Option[S]]), Set[Lookahead[Option[S]]]]()

  type Generator[I[A] <: Item[I, A], S, T] = Reader[GeneratorEnv[I, S], T]

  type Conflict[T] = (StateI, List[List[(Token[T], Action[T])]])

  implicit def MetaSlrItem = SlrItem

  implicit def MetaLalrItem = LalrItem

  def goto[I[T] <: Item[I, T]: MetaItem, S](is: Set[I[S]], sym: Symbol[S]): Set[I[S]] = {
    def nextTest(sym: Symbol[S], item: I[S]): Option[I[S]] =
      if(item.nextSymbol == Tok(sym))
        Some(item.next)
      else
        None
      implicitly[MetaItem[I]].closure(for {
        i <- is
        s <- nextTest(sym, i)
      } yield s)
  }

  def itemSets[I[T] <: Item[I, T]: MetaItem, S](rid: RuleId[S], syms: Set[Symbol[S]]): Set[Set[I[S]]] = {
    def aux(c: Set[Set[I[S]]]): (Set[Set[I[S]]], Set[Set[I[S]]]) = {
      val gotos = for {
        s <- syms
        i <- c
      } yield goto(i, s)
      (c ++ gotos, gotos)
    }
    val meta = implicitly[MetaItem[I]]
    Set(meta.closure(Set(meta.startItem(rid)))).recTraverseG(aux).filterNot(_.isEmpty)
  }

  def precomputeGotos[I[T] <: Item[I, T]: MetaItem, S](items: List[(Set[I[S]], StateI)], itemsIdx: Map[Set[I[S]], StateI], syms: List[Symbol[S]]): Map[(StateI, Symbol[S]), StateI] =
    (for {
      (itemSet, id) <- items
      sym <- syms
      st <- lookupItemSet(items, itemsIdx, goto(itemSet, sym))
    } yield ((id, sym), st)).toMap

  private def lookupItemSet[I[T] <: Item[I, T], S](items: List[(Set[I[S]], StateI)], itemsIdx: Map[Set[I[S]], StateI], s: Set[I[S]]): Option[StateI] =
    if(s.isEmpty) {
      None
    } else itemsIdx.get(s) match {
      case None =>
        items.filter(_._1.subsetOf(s)).headOption.map(_._2)
      case s =>
        s
    }

  def askItemSet[I[T] <: Item[I, T], S](s: Set[I[S]]): Generator[I, S, Option[StateI]] =
    for {
      items <- Reader.ask((env: GeneratorEnv[I, S]) => env.itemSets)
      itemsIdx <- Reader.ask((env: GeneratorEnv[I, S]) => env.itemSetIndex)
    } yield lookupItemSet(items, itemsIdx, s)

  def askGoto[I[T] <: Item[I, T], S](st: StateI, sym: Symbol[S]): Generator[I, S, Option[StateI]] =
    for(g <- Reader.ask((env: GeneratorEnv[I, S]) => env.gotos))
      yield g.get(st -> sym)

}
