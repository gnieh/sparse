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

sealed trait ParseResult[+T, +A] {

  def map[B](f: A => B): ParseResult[T, B] = this match {
    case OkResult(value)               => OkResult(f(value))
    case ErrorResult(expected, offset) => ErrorResult(expected, offset)
  }

  def flatMap[S >: T, B](f: A => ParseResult[S, B]): ParseResult[S, B] = this match {
    case OkResult(value)               => f(value)
    case ErrorResult(expected, offset) => ErrorResult(expected, offset)
  }

}

final case class ErrorResult[T](expected: List[Token[T]], offset: Int) extends ParseResult[T, Nothing]

final case class OkResult[A](value: A) extends ParseResult[Nothing, A]
