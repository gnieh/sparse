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
package untyped

import util._

sealed trait Symbol[S] {

  def map[T](f: S => T): Done[RuleId[S], RuleId[T], Symbol[T]]

}

final case class TerminalSymbol[S](t: S) extends Symbol[S] {

  def map[T](f: S => T): Done[RuleId[S], RuleId[T], Symbol[T]] =
    State.unit(TerminalSymbol(f(t)))

}

final case class RuleSymbol[S](rid: RuleId[S]) extends Symbol[S] {

  def map[T](f: S => T): Done[RuleId[S], RuleId[T], Symbol[T]] =
    for {
      done <- Done.get(rid)
      a <- done match {
        case Some(r) =>
          State.unit[Map[RuleId[S], RuleId[T]], Symbol[T]](RuleSymbol(r))
        case None =>
          for {
            res <- rid.mapRule(f)
            () <- Done.put(rid, res)
          } yield RuleSymbol(res)
      }
    } yield a

}
