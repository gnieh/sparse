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

package object util {

  type Done[K, V, A] = State[Map[K, V], A]

  type DoneA[K, V] = Done[K, V, V]

  type MultiMap[K, V] = Map[K, Set[V]]

  object Done {

    def get[K, V](k: K): Done[K, V, Option[V]] =
      State.get[Map[K, V], Option[V]](_.get(k))

    def put[K, V](k: K, v: V): Done[K, V, Unit] =
      State.put(_.updated(k, v))

  }

  implicit class DoneOperator[K, V, A](val action: Done[K, V, A]) extends AnyVal {

    def ifNotDoneG(k: K, ifDone: V => A): Done[K, V, A] =
      for {
        done <- Done.get[K, V](k)
        a <- done match {
          case Some(v) => State.unit[Map[K, V], A](ifDone(v))
          case None    => action
        }
      } yield a

    def eval: A =
      action.run(Map.empty)._1

  }

  implicit class DoneAOperator[K, V](val action: DoneA[K, V]) extends AnyVal {

    def ifNotDone(k: K): DoneA[K, V] =
      action.ifNotDoneG(k, identity[V])

  }

  implicit class SetOperator[A](val set: Set[A]) extends AnyVal {

    def recTraverseG[B](f: Set[A] => (Set[B], Set[A])): Set[B] = {
      def aux(done: Set[A], x: Set[A]): Set[B] = {
        val (res, cand) = f(x)
        val done1 = done ++ x
        val cand1 = cand -- done1
        if(cand1.isEmpty) {
          res
        } else {
          res ++ aux(done1, cand1)
        }
      }
      aux(Set.empty, set)
    }

  }

  implicit class PairSetOperator[K, V](val set: Set[(K, V)]) extends AnyVal {

    def toMultiMap: MultiMap[K, V] =
      set.foldLeft(Map[K, Set[V]]()) {
        case (acc, (k, v)) => acc.addBinding(k, v)
      }

  }

  implicit class MultiMapOperator[K,V](val map: MultiMap[K, V]) extends AnyVal {

    def lookup(k: K): Set[V] =
      map.get(k).getOrElse(Set.empty)

    def addBinding(k: K, v: V): MultiMap[K, V] =
      map.updated(k, lookup(k) + v)

    def addBindings(k: K, s: Set[V]): MultiMap[K, V] =
      map.updated(k, lookup(k) ++ s)

    def removeBinding(k: K, v: V): MultiMap[K, V] = {
      val set = map.lookup(k) - v
      if(set.isEmpty)
        map - k
      else
        map.updated(k, set)
    }

    def union(that: MultiMap[K, V]): MultiMap[K, V] =
      that.foldLeft(map) {
        case (acc, (k, s)) => acc.addBindings(k, s)
      }

  }

}
