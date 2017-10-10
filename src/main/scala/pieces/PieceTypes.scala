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
}

/**
  * [[Moved]] and [[Unmoved]] are flags used for [[PawnPieceType]], [[RookPieceType]] and [[KingPieceType]]
  * Since these pieces have special moves before they are ever moved then they have these extending traits
  */
trait Moved

trait Unmoved