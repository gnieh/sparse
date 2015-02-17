package sparse

import scala.language.experimental.macros

import scala.annotation.compileTimeOnly

import driver._

abstract class Parser[Token] {

  val lexer: Lexer[Token]

  @compileTimeOnly("`term` may only be used in the `Grammar` section of a parser")
  def term[T <: Token]: Symbol[T] = ???

  @compileTimeOnly("`empty` may only be used in the `Grammar` section of a parser")
  def empty: Symbol[Nothing] = ???

  def Grammar(rules: Any): Any =
    macro ParserImpl.Grammar

}
