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

object Lalr {

  private def items[S]: Generator[SlrItem, Option[S], List[(Set[LalrItem[Option[S]]], Int)]] =
    for {
      start <- Reader.ask((env: GeneratorEnv[SlrItem, Option[S]]) => env.startRule)
      itemSets <- Reader.ask((env: GeneratorEnv[SlrItem, Option[S]]) => env.itemSets)
      kernelss = itemSets.map(p => (p._1.filter(_.isKernel(start)), p._2))
      syms <- Reader.ask((env: GeneratorEnv[SlrItem, Option[S]]) => env.symbols)
      lookaheads <- Reader.sequence(itemSets.zip(kernelss).map {
        case ((_, state), (kernels, _)) =>
          Reader.mapM((s: Symbol[Option[S]]) => Lookahead.compute(state, kernels, s))(syms)
      })
      table = lookaheads.flatten.foldLeft(LookaheadTable[S])(_.union(_))
      newitems = for {
        (kernels, state) <- kernelss
        item <- kernels
      } yield {
        val newitems = items(item, kernels, state, table)
        (LalrItem.closure(newitems.toSet), state)
      }
    } yield newitems

  private def items[S](item: SlrItem[Option[S]], kernels: Set[SlrItem[Option[S]]], state: StateI, table: LookaheadTable[S]): Set[LalrItem[Option[S]]] =
    for {
      k <- kernels
      token <- Lookahead.find(table, state, item).map(tokens => for {
        token <- tokens
        if token != Tok(None)
      } yield item.toLalr(token)).eval
    } yield token

  private def fromSlrGenerator[S](slrEnv: GeneratorEnv[SlrItem, Option[S]]): GeneratorEnv[LalrItem, Option[S]] = {
    val newItems = items.runReader(slrEnv)
    val newItemsMap = newItems.toMap
    slrEnv.copy(itemSets = newItems,
      itemSetIndex = newItemsMap,
      gotos = precomputeGotos(newItems, newItemsMap, slrEnv.symbols))
  }

  def createTables[S](startRid: RuleId[S]): (ActionTable[S], GotoTable[S], StateI) = {
    val initSlrEnv = GeneratorEnv.create[SlrItem, Option[S]](startRid.map(Some(_)))
    val initGen = fromSlrGenerator(initSlrEnv)
    val items = initGen.itemSets
    val acts = for((item, state) <- items) yield actions(item, state).runReader(initGen)
    val gots = (for((_, state) <- items) yield gotos(state).runReader(initGen)).flatten
    (acts, gots, initGen.startState)
  }

  private def gotos[S](state: StateI): Generator[LalrItem, Option[S], List[((StateI, RuleI), StateI)]] =
    for {
      nt <- Reader.ask((env: GeneratorEnv[LalrItem, Option[S]]) => env.nonTerminals)
      opts <- Reader.sequence(for {
        r @ RuleSymbol(RuleId(id, _)) <- nt
      } yield askGoto[LalrItem, Option[S]](state, r).map(s => ((state, id), s)))
    } yield for(((i, ai), Some(s)) <- opts) yield ((i, ai), s)

  private def actions[S](items: Set[LalrItem[Option[S]]], state: StateI): Generator[LalrItem, Option[S], (StateI, (List[(Token[S], Action[S])], Action[S]))] = {
    def reduces(t: List[(Token[S], Action[S])]) =
      t.collect {
        case (_, r @ Reduce(_, _, _, _)) => r
      }
    def shifts(t: List[(Token[S], Action[S])]) =
      t.collect {
        case (t, Shift(_)) => t
      }
    def addShifts(tokens: List[Token[S]], a: Action[S]) = a match {
      case Reduce(rule, prod, stack, _) => Reduce(rule, prod, stack, tokens)
      case _                            => a
    }
    def mapShifts(t: List[(Token[S], Action[S])]) =
      for((ts, a) <- t)
        yield (ts, addShifts(shifts(t), a))
    def define(t: List[(Token[S], Action[S])]) = {
      val reds = reduces(t)
      if(reds.isEmpty)
        Error(shifts(t))
      else
        reds.head
    }

    for {
      start <- Reader.ask((env: GeneratorEnv[LalrItem, Option[S]]) => env.startRule)
      t <- Reader.sequence(for(it <- items.toList) yield actions1(it, start.id, state))
      ms = mapShifts(t.flatten)
    } yield (state, (ms, define(ms)))
  }

  private def actions1[S](item: LalrItem[Option[S]], rule: RuleI, state: StateI): Generator[LalrItem, Option[S], List[(Token[S], Action[S])]] = item.nextSymbol match {
    case Tok(a @ TerminalSymbol(Some(s))) =>
      for(st <- askGoto(state, a))
        yield st.map(st => (Tok(s), Shift(st))).toList
    case EOI =>
      if(item.id.id != rule)
        Reader.unit(
          List((item.lookahead.map(_.get),
            Reduce(item.id.id, item.prod, item.id.rule.size, Nil))))
      else if(item.lookahead == EOI)
        Reader.unit(List((EOI, Accept)))
      else
        Reader.unit(Nil)
    case _ =>
      Reader.unit(Nil)
  }

}
