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

import scala.language.implicitConversions

package object grammar {

  type Rule[T, A] = List[Production[T, A]]

  type GrammarState[T, A] = State[RuleIds[T], A]

  type Grammar[T, A] = GrammarState[T, RuleId[T, A]]

  @inline
  implicit def tToSym[T](v: T): Symbol[T, T] =
    TerminalSymbol(v)

  @inline
  implicit def ridToSym[T, A](id: RuleId[T, A]): Symbol[T, A] =
    RuleSymbol(id)

  implicit class SeqOperator[T, A, B](val p: Production[T, B => A]) extends AnyVal {

    @inline
    def ~(s: =>Symbol[T, B]): Production[T, A] =
      SeqProduction(p, s)

  }

  implicit class SeqNOperator[T, A](val p: Production[T, A]) extends AnyVal {

    @inline
    def <~[C](s: =>Symbol[T, C]): Production[T, A] =
      SeqNProduction(p, s)

  }

  implicit class FunOperator[A, B](val f: B => A) extends AnyVal {

    @inline
    def ^^[T](s: Symbol[T, B]): Production[T, A] =
      SeqProduction(FunProduction(f), s)

  }

  implicit class ValOperator[A](val v: A) extends AnyVal {

    @inline
    def ^^^[T, B](s: Symbol[T, B]): Production[T, A] =
      SeqNProduction(FunProduction(v), s)

  }

  implicit class GrammarOperator[T, A](val g: Grammar[T, A]) extends AnyVal {

    /** Creates and returns the augmented grammar which has a new start symbol. */
    def augment: Grammar[T, A] =
      for {
        r <- g
        s <- rule(List(identity[A] _ ^^ r))
      } yield s

  }

  implicit class GrammarStateOperator[T, A](val g: GrammarState[T, A]) extends AnyVal {

    def evalGrammar: A =
      g.run(RuleIds(Stream.from(0)))._1

  }

  def rule[T, A](r: Rule[T, A]): Grammar[T, A] =
    for {
      st <- State.get[RuleIds[T]]
      i #:: is = st.rules
      _ <- State.put(RuleIds[T](is))
    } yield RuleId(i, r)

  @inline
  def epsilon[A](v: A): Production[Nothing, A] =
    FunProduction(v)

  @inline
  def lambda[A](v: A): Production[Nothing, A] =
    epsilon(v)

  @inline
  def lamda[A](v: A): Production[Nothing, A] =
    epsilon(v)

  @inline
  def ε[A](v: A): Production[Nothing, A] =
    epsilon(v)

  @inline
  def Λ[A](v: A): Production[Nothing, A] =
    epsilon(v)

}
