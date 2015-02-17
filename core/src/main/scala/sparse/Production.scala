package sparse

import shapeless._

abstract class Production[Token,Result] extends Symbol[Result] {

  def firsts: Set[Int]

  def nullable: Boolean

  def reduce(stack: HList): Result :: HList

}
