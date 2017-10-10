import board.{Board, Position}


class PieceMover(implicit board: Board) {

  /**
    * this method moves pieces around
    */
  def move(from: Position, to: Position): Unit = {
    val (opf,opt) = board.pieceAt(from) -> board.pieceAt(to)
    (opf,opt) match {
      case (None,_) => throw new IllegalMoveException(s"No piece to move at $from")
      case (Some(f), Some(t)) if f.color == t.color => throw new IllegalMoveException(s"You cannot move a piece on it's own color")
      case (Some(f), Some(t)) if f.color != t.color && checkLegalMove(from: Position, to: Position) => ???
    }
  }

  def checkLegalMove(from: Position, to: Position): Boolean = ???
}

class IllegalMoveException(val message: String) extends Exception