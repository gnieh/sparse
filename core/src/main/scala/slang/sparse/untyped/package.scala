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

import scala.annotation.tailrec

package object untyped {

  type Rule[S] = List[Production[S]]

  type Production[S] = List[Symbol[S]]

  type UntypedState[S, A] = State[(Map[Int, RuleId[S]], ProdFunTable), A]

  def terminals[S](rs: List[RuleId[S]]): List[Symbol[S]] =
    for {
      r <- rs
      as <- r.rule
      s @ TerminalSymbol(_) <- as
    } yield s

  def nonTerminals[S](rs: List[RuleId[S]]): List[Symbol[S]] =
    for {
      r <- rs
      as <- r.rule
      s @ RuleSymbol(_) <- as
    } yield s

  implicit class ProductionOperator[S](val prod: Production[S]) extends AnyVal {

    def acceptEpsilon: Boolean =
      prod.isEmpty || prod.forall {
        case TerminalSymbol(_) => false
        case RuleSymbol(rid)   => rid.acceptEpsilon
      }

    def firsts: Set[EToken[S]] = {
      @tailrec
      def loop(prod: Production[S], acc: Set[EToken[S]]): Set[EToken[S]] = prod match {
        case Nil =>
          acc
        case TerminalSymbol(t) :: _ =>
          acc + ETok(t)
        case RuleSymbol(rid) :: rest =>
          val acc1 = rid.firsts
          if(acc1.contains(Epsilon))
            loop(rest, acc ++ acc1)
          else
            acc ++ acc1
      }
      loop(prod, Set.empty)
    }
  }

}
