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
package grammar

import scala.annotation.tailrec

sealed abstract class Production[+T, A] {

  def semanticAction: DynFun = {
    @tailrec
    def aux(p: Production[T, _], as: List[Boolean]): DynFun = p match {
      case FunProduction(f)     => DynFun(f, as)
      case SeqProduction(p, _)  => aux(p, true :: as)
      case SeqNProduction(p, _) => aux(p, false :: as)
    }
    aux(this, Nil)
  }

}

final case class SeqProduction[T, A, B](prefix: Production[T, B => A], sym: Symbol[T, B]) extends Production[T, A]

final case class SeqNProduction[T, A, B](prefix: Production[T, A], sym: Symbol[T, B]) extends Production[T, A]

final case class FunProduction[A](v: A) extends Production[Nothing, A]
