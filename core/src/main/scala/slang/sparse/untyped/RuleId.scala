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

import sparse.{ grammar => T }

import scala.annotation.tailrec

/** An untyped rule.
 *  It consists of a [[sparse.RuleI]] and a list of [[sparse.untyped.Production]]s.
 *
 *  @author Lucas Satabin
 */
final case class RuleId[S](id: RuleI, rule: Rule[S]) extends Ordered[RuleId[S]] {

  def compare(that: RuleId[S]): Int =
    this.id - that.id

  override def equals(that: Any): Boolean = that match {
    case RuleId(id1, _) => id == id1
    case _              => false
  }

  def mapRule[T](f: S => T): DoneA[RuleId[S], RuleId[T]] =
    State.mapM(State.mapM((s: Symbol[S]) => s.map(f)))(rule).map(r => RuleId(id, r))

  def map[T](f: S => T): RuleId[T] =
    mapRule(f).run(Map.empty)._1

  /** Returns the rules from grammar following rules recursively and starting from this one. */
  lazy val rules: List[RuleId[S]] = {
    def aux(rid: RuleId[S]): Set[RuleId[S]] =
      (for {
        p <- rid.rule
        RuleSymbol(r) <- p
      } yield r).toSet
    def rules1(rs: Set[RuleId[S]]) = {
      val res = rs.map(aux _).flatten
      (res ++ rs, res)
    }
    Set(this).recTraverseG(rules1).toList
  }

  /** Indicates whether this rule accept epsilon. */
  def acceptEpsilon: Boolean =
    rule.isEmpty || rule.exists(p => p.acceptEpsilon)

  /** The set of tokens that can appear first in this rule (including epsilon). */
  lazy val firsts: Set[EToken[S]] = {
    @tailrec
    def loop(rids: Set[RuleId[S]], acc: Set[EToken[S]], seen: Set[RuleId[S]]): Set[EToken[S]] = {
      if(rids.isEmpty) {
        acc
      } else {
        val rid = rids.head
        val rest = rids.tail
        if(seen.contains(rid)) {
          loop(rest, acc, seen)
        } else {
          val acc1 = rid.rule.foldLeft(acc) { (acc1, prod) =>
            acc1 ++ prod.firsts
          }
          if(rid.acceptEpsilon) {
            loop(rest, acc1 + Epsilon, seen + rid)
          } else {
            loop(rest, acc1, seen + rid)
          }
        }
      }
    }
    loop(Set(this), Set.empty, Set.empty)
  }

  /** The set of tokens that can follow this rule, given the starting rule `rule` in the grammar
   *  and the set of all rules `rules`. */
  def follow(rid: RuleId[S], start: RuleId[S], rules: List[RuleId[S]]): Set[Token[S]] =
    follow1(rid, start, rules).run(Map.empty)._1

  private def follow1(rid: RuleId[S], start: RuleId[S], rules: List[RuleId[S]]): Done[RuleId[S], Unit, Set[Token[S]]] = {
    def followProduction(a: RuleId[S], p: Production[S]): Done[RuleId[S], Unit, Set[Token[S]]] = p match {
      case Nil =>
        State.unit(Set.empty)
      case s :: rest if s == RuleSymbol(rid) =>
        // compute the firsts set for the rest of the production
        val firstsRest = rest.firsts
        val rest1 = for {
          ETok(t) <- firstsRest
        } yield Tok(t)
        for {
          // compute the follow set for rule `a' if the rest of the production accepts epsilon
          followa <-
            if(firstsRest.contains(Epsilon))
              follow1(a, start, rules)
            else
              State.unit[Map[RuleId[S], Unit], Set[Token[S]]](Set.empty)
          // compute the follow set for the rest of the production
          followrest <- followProduction(a, rest)
        } yield followa ++ followrest ++ rest1
      case _ :: rest =>
        followProduction(a, rest)
    }
    val done = for {
      () <- Done.put(rid, ())
      toSequence = for {
        a @ RuleId(_, prods) <- rules
        prod <- prods
      } yield followProduction(a, prod)
      l <- State.sequence(toSequence)
      prods = l.toSet.flatten
    } yield if(rid == start) prods + EOI else prods
    done.ifNotDoneG(rid, _ => Set.empty)
  }

}

object RuleId {

  def fromTypedRule[S1, S2, A](transform: S1 => S2)(typed: T.RuleId[S1, A]): (RuleId[S2], ProdFunTable) = {
    val (rid, rules) = untypeRule(transform)(typed).run(Map.empty -> Nil)
    (rid, rules._2)
  }

  private def untypeRule[S1, S2, A](c: S1 => S2)(typed: T.RuleId[S1, A]): UntypedState[S2, RuleId[S2]] = {
    def doRule(rids: Map[RuleI, RuleId[S2]], funs: ProdFunTable, typed: T.RuleId[S1, A]): UntypedState[S2, RuleId[S2]] =
      rids.get(typed.id) match {
        case Some(rule) =>
          State.unit(rule)
        case None =>
          val newfuns = List.fill(typed.rule.size)(typed.id).zipWithIndex.zip(typed.rule.map(_.semanticAction))
          for {
            r <- State.mapM(untypeProduction[S1, S2, A](c))(typed.rule)
            res = RuleId(typed.id, r)
            () <- State.put((rids.updated(typed.id, res), funs ++ newfuns))
          } yield res
      }
    for {
      (rids, funs) <- State.get[(Map[RuleI, RuleId[S2]], ProdFunTable)]
      r <- doRule(rids, funs, typed)
    } yield r
  }

  private def untypeProduction[S1, S2, A](c: S1 => S2)(p: T.Production[S1, A]): UntypedState[S2, Production[S2]] =
    p match {
      case T.SeqProduction(ps, s) =>
        for {
          sym <- untypeSymbol(c, s).map(List(_))
          prod <- untypeProduction(c)(ps)
        } yield prod ++ sym
      case T.SeqNProduction(ps, s) =>
        for {
          sym <- untypeSymbol(c, s).map(List(_))
          prod <- untypeProduction(c)(ps)
        } yield prod ++ sym
      case T.FunProduction(_) =>
        State.unit(Nil)
    }

  private def untypeSymbol[S1, S2, A](c: S1 => S2, s: T.Symbol[S1, A]): UntypedState[S2, Symbol[S2]] =
    s match {
      case T.TerminalSymbol(t) => State.unit(TerminalSymbol[S2](c(t)))
      case T.RuleSymbol(r)     => untypeRule(c)(r).map(r => RuleSymbol[S2](r))
    }

}
