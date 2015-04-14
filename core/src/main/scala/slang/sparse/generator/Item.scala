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

abstract class Item[I[T] <: Item[I, T], S] {

  val id: RuleId[S]

  val prod: ProdI

  val pos: Int

  /** Indicates whether this item is a kernel item. */
  def isKernel(start: RuleId[S]): Boolean =
    pos > 0 || (id == start && pos == 0)

  /** Returns the symbol appearing right after `pos` in this item.
   *  If `pos` is at the end, returns `EOI`. */
  def nextSymbol: Token[Symbol[S]] =
    if(pos < id.rule(prod).size)
      Tok(id.rule(prod)(pos))
    else
      EOI

  /** Returns the next item created from this one. */
  def next: I[S]

}

abstract class MetaItem[I[T] <: Item[I, T]] {

  def closure[S](set: Set[I[S]]): Set[I[S]]

  def startItem[S](r: RuleId[S]): I[S]


}
