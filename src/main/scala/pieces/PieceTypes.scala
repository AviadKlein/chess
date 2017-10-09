package pieces

trait PieceType {
  val pieceType: String
}

trait KingPieceType extends PieceType {
  val pieceType: String = "King"
}

trait QueenPieceType extends PieceType {
  val pieceType: String = "Queen"
}

trait BishopPieceType extends PieceType {
  val pieceType: String = "Bishop"
}

trait KnightPieceType extends PieceType {
  val pieceType: String = "Knight"
}

trait RookPieceType extends PieceType {
  val pieceType: String = "Rook"
}

trait PawnPieceType extends PieceType {
  val pieceType: String = "Pawn"
  private var moved: Boolean = false
  def canMove2: Boolean = !moved
}