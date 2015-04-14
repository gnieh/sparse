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
package slang.util

final case class State[S, +A](run: S => (A, S)) extends Monad[State.T[S]#Type, A] {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      (f(a), t)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      f(a).run(t)
    })

  def filter(f: A => Boolean): State[S, A] =
    State(s => {
      val (a, t) = run(s)
      if(f(a))
        (a, t)
      else
        throw new IllegalStateException("Boolean predicate does not hold on state")
    })

}

object State {

  type T[S] = {
    type Type[+A] = State[S, A]
  }

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def get[S]: State[S, S] =
    State(s => (s, s))

  def get[S, A](f: S => A): State[S, A] =
    State(s => (f(s), s))

  def put[S](v: S): State[S, Unit] =
    State(s => ((), s))

  def put[S](f: S => S): State[S, Unit] =
    State(s => ((), f(s)))

  def mapM[A, B, S](f: A => State[S, B])(l: List[A]): State[S, List[B]] =
    l.foldRight(State.unit[S, List[B]](Nil)) { (a, acc) =>
      for {
        b <- f(a)
        bs <- acc
      } yield b :: bs
    }

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(State.unit[S, List[A]](Nil)) { (a, acc) =>
      for {
        first <- a
        as <- acc
      } yield first :: as
    }

}
