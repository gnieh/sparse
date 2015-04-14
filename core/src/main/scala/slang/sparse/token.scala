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

sealed trait Token[+T] {

  def map[U](f: T => U): Token[U] =
    this match {
      case Tok(t) => Tok(f(t))
      case EOI    => EOI
    }

}

final case class Tok[T](t: T) extends Token[T] {

  override def toString = t.toString

}

case object EOI extends Token[Nothing] {

  override def toString = "EOI"

}

sealed trait EToken[+T] {

  def map[U](f: T => U): EToken[U] =
    this match {
      case ETok(t) => ETok(f(t))
      case Epsilon   => Epsilon
    }

}

final case class ETok[T](t: T) extends EToken[T]

case object Epsilon extends EToken[Nothing]
