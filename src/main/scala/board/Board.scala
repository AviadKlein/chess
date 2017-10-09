package board

import pieces._

class Board {

  /**
    * The board is represented by a 64 long array of optional pieces
    * position 0 in the array represents A1 (left most bottom)
    * position 1 represents B1 (next file, same rank)
    * position 63 represents H8 ... and so on
    */
  final private lazy val board = Array.fill[Option[Piece]](64)(None)
  final private lazy val indices = board.indices

  /**
    * a linear section is a sequence of adjoining [[Position]]s in either a File, Rank or Diagonal
    */
  type LinearSection = Seq[Option[Piece]]

  /**
    * We update a single position with an optional piece
    */
  private def update(position: Position, pieceOp: Option[Piece]): Unit = board.update(position.index, pieceOp)
  private def update(index: Int, pieceOp: Option[Piece]): Unit = update(Position(index), pieceOp)
  def update(position: Position, piece: Piece): Unit = update(position, Some(piece))
  def clear(position: Position): Unit = update(position, None)


  /**
    * we clear the board from all pieces
    */
  def clear(): Unit = indices.foreach(i => update(i, None))

  /**
    * We place the pieces in their starting positions
    */
  def restart(): Unit = {
    clear()
    Pieces.whiteInit.foreach{case (piece, i) => update(i, Some(piece))}
    Pieces.blackInit.foreach{case (piece, i) => update(i, Some(piece))}
  }

  /**
    * Check if position are empty/taken
    */
  private def positionEmpty(position: Position): Boolean = board(position.index).isEmpty
  def positionEmpty(r: Int, f: Int): Boolean = positionEmpty(Position(r,f))
  private def positionTaken(position: Position): Boolean = board(position.index).isDefined
  def positionTaken(r: Int, f: Int): Boolean = positionTaken(Position(r,f))

  /**
    * Return an optional piece in a position
    */
  private def pieceAt(position: Position): Option[Piece] = board(position.index)
  def pieceAt(r: Int, f: Int): Option[Piece] = board(Position(r,f).index)

  /**
    * We return the full rank/file by it's index
    */
  def rank(i: Int): LinearSection = {
    require(i >= 1 && i <= 8, f"invalid rank, got $i")
    (0 to 7).map(file => board(file + (i-1)*8))
  }

  def rank(p1: Position, p2: Position): LinearSection = {
    require(p1.rank == p2.rank, f"positions's rank must be the same, got ($p1, $p2)")
    (p1.file to p2.file).map(f => pieceAt(p1.rank, f))
  }

  def file(p1: Position, p2: Position): LinearSection = {
    require(p1.file == p2.file, f"positions's file must be the same, got ($p1, $p2)")
    (p1.rank to p2.rank).map(r => pieceAt(r, p1.file))
  }

  def file(i: Int): LinearSection = {
    require(i >= 1 && i <= 8, f"invalid file, got $i")
    (0 to 7).map(rank => board((i-1) + 8*rank))
  }


  def diagIteration(acc: Seq[Position], ranks: Int, files: Int): Seq[Position] =
    (ranks, files) match {
      case (0, 0) => acc
      case (r, f) if r > 0 && f > 0 => diagIteration(acc :+ acc.last.diagRightUp, r-1,f-1)
      case (r, f) if r < 0 && f > 0 => diagIteration(acc :+ acc.last.diagRightDown, r+1,f-1)
      case (r, f) if r > 0 && f < 0 => diagIteration(acc :+ acc.last.diagLeftUp, r-1,f+1)
      case (r, f) if r < 0 && f < 0 => diagIteration(acc :+ acc.last.diagLeftDown, r+1,f+1)
    }


  /**
    * Returns a [[LinearSection]] from the board starting from
    */
  def diag(p1: Position, p2: Position): LinearSection = {
    require(p1 diagonalTo p2, s"positions are not diagonal to one another, got ($p1, $p2)")
    val (r,f) = p1 manhattanSteps p2
    diagIteration(p1 :: Nil,r,f).map(pieceAt)
  }

  def render: String =
    (1 to 8).reverse.map(i => rank(i).map(op => PieceRenderer(op)).mkString("|","|","|")).mkString("\n")

}

private [board] case class Position(rank: Int, file: Int) {
  require(rank >= 1 && rank <= 8, s"Invalid position, got ($rank, $file)")
  require(file >= 1 && file <= 8, s"Invalid position, got ($rank, $file)")
  val index = (rank-1)*8  +file - 1

  def manhattanSteps(to: Position): (Int, Int) =
    (to.rank - this.rank, to.file - this.file)

  def diagonalTo(that: Position): Boolean =
    math.abs(this.rank - that.rank) == math.abs(this.file - that.file)

  def diagRightUp = Position(rank + 1, file + 1)
  def diagRightDown = Position(rank - 1, file + 1)
  def diagLeftUp = Position(rank + 1, file - 1)
  def diagLeftDown = Position(rank - 1, file - 1)
}

private [board] object Position {
  def apply(index: Int): Position = {
    require(index >= 0 && index <= 63, s"index must be between 0 and 63, got $index")
    Position(index / 8 + 1,index % 8 + 1)
  }
}