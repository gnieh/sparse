package sparse

import shapeless._

import scala.reflect._

import scala.annotation.tailrec

import scala.util.{
  Try,
  Success,
  Failure
}

sealed trait Action
final case class Shift(next: Int) extends Action
final case class Reduce[Token,Out](p: Production[Token,Out]) extends Action
case object Accept extends Action
case object Error extends Action

class Table[T] private(val rows: Int, columns: Int, nnz: Array[T], ia: Array[Int], ja: Array[Int]) {

  def get(row: Int, col: Int): Option[T] = {
    val view = ja.view(ia(row), ia(row + 1))
    @tailrec
    def find(idx: Int): Int = {
      val elem = view(idx)
      if(elem > col)
        -1
      else if(elem == col)
        idx
      else
        find(idx + 1)
    }
    val idx = find(0)
    if(idx == -1)
      None
    else
      Some(nnz(idx))
  }

  @inline
  def get(row: Int, col: Int, default: =>T): T =
    get(row, col).getOrElse(default)

}

object Table {

  def EmptyTable[T: ClassTag] =
    new Table[T](0, 0, Array(), Array(), Array())

  def fromList[T: ClassTag](rows: Int, cols: Int, matrix: List[(Int, List[(Int, T)])]): Table[T] = {
    val nnzSize = matrix.foldLeft(0) {
      case (acc, (_, l)) => acc + l.size
    }
    if(nnzSize == 0) {
      EmptyTable[T]
    } else  {
      val nnz = Array.ofDim[T](nnzSize)
      val ia = Array.ofDim[Int](rows + 1)
      ia(rows) = nnzSize
      val ja = Array.ofDim[Int](cols)
      val sortedRows = matrix.sortBy(_._1)
      @tailrec
      def loopIa(currentIdx: Int, matrix: Seq[(Int, Seq[(Int, T)])]): Unit = matrix match {
        case Seq((idx, row), rest @ _*) =>
          ia(idx) = currentIdx
          loopIa(currentIdx + row.size, rest)
        case Seq() =>
          ()
      }
      loopIa(0, matrix)
      @tailrec
      def loopJa(currentIdx: Int, row: Seq[(Int, T)]): Unit = row match {
        case Seq((idx, _), rest @ _*) =>
          ja(idx) = currentIdx
          loopJa(currentIdx + 1, rest)
        case Seq() =>
          ()
      }
      for((_, row) <- sortedRows)
        loopJa(0, row)

      new Table(rows, cols, nnz, ia, ja)
    }
  }

}

abstract class Automaton[Token,Out: ClassTag](action: Table[Action], goto: Table[Int]) {

  val Ct = classTag[Out]

  def run(lexer: Lexer[Token]): Try[Out] =
    run(lexer, 0 :: HNil)

  def asTerminal[T <: Token](token: T): Terminal[T]

  private object State {
    def unapply(a: Any): Option[Int] =
      a match {
        case i: Int if i >= 0 => Some(i)
        case _                => None
      }
  }

  @tailrec
  private def run(lexer: Lexer[Token], stack: HList): Try[Out] =
    stack match {
      case -1 :: _ =>
        Failure(new ParserInternalException)
      case State(state) :: stack =>
        action.get(state, asTerminal(lexer.peek()).id, Error) match {
          case Shift(next) =>
            run(lexer, next :: lexer.next() :: stack)
          case Reduce(p) =>
            run(lexer, goto.get(state, p.id, -1) :: p.reduce(stack))
          case Accept =>
            stack match {
              case Ct(o) :: _ => Success(o)
              case _          => Failure(new ParserInternalException)
            }
          case Error =>
            Failure(new Exception)
        }
      case _ =>
        Failure(new ParserInternalException)
    }

}

class ParserInternalException extends Exception
