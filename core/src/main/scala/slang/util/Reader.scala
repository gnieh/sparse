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
package util

final case class Reader[Env,+T](runReader: Env => T) extends Monad[Reader.T[Env]#Type, T] {

  def map[S](f: T => S): Reader[Env, S] =
    Reader(runReader.andThen(f))

  def flatMap[S](f: T => Reader[Env, S]): Reader[Env, S] =
    Reader(e => f(runReader(e)).runReader(e))

  def filter(p: T => Boolean): Reader[Env, T] =
    Reader(e => {
      val t = runReader(e)
      if(p(t))
        t
      else
        throw new IllegalStateException("Boolean predicate does not hold on reader")
    })

}

object Reader {

  type T[Env] = {
    type Type[+T] = Reader[Env, T]
  }

  def unit[Env, T](t: T): Reader[Env, T] =
    Reader(_ => t)

  def ask[Env]: Reader[Env, Env] =
    Reader(identity[Env])

  def ask[Env, T](f: Env => T): Reader[Env, T] =
    for {
      e <- ask[Env]
      r <- unit(f(e))
    } yield r

  def local[Env, T](update: Env => Env)(f: Env => T): Reader[Env, T] =
    Reader(e => f(update(e)))

  def mapM[Env, T, S](f: S => Reader[Env, T])(l: List[S]): Reader[Env, List[T]] =
    l.foldRight(Reader.unit[Env, List[T]](Nil)) { (a, acc) =>
      for {
        b <- f(a)
        bs <- acc
      } yield b :: bs
    }

  def sequence[Env, T](l: List[Reader[Env, T]]): Reader[Env, List[T]] =
    l.foldRight(Reader.unit[Env, List[T]](Nil)) { (a, acc) =>
      for {
        first <- a
        as <- acc
      } yield first :: as
    }
}
