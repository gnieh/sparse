package sparse

trait Lexer[Token] {

  /** Reads and consumes the next token and returns it.
   *  `EOI` is returned if the end of input was reached. */
  def next(): Token

  /** Reads and the next token without consuming it and returns it.
   *  `EOI` is returned if the end of input was reached. */
  def peek(): Token

  /** The token returned when end of input was reached. */
  def EOI: Token

}
