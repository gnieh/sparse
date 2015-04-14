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

sealed abstract class Action[+S] {
  val isReduce: Boolean = false
}

case object Accept extends Action[Nothing] 

final case class Error[S](t: List[Token[S]]) extends Action[S]

final case class Reduce[S](rule: RuleI, prod: ProdI, stack: StackI, tokens: List[Token[S]]) extends Action[S] {
  override val isReduce = true
}

final case class Shift(state: StateI) extends Action[Nothing]