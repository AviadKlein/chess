package pieces

trait Piece {
  val name: String
  val color: Byte
}

trait WhitePiece extends Piece with PieceType {
  val color = 0
  val name = "White " + pieceType
}

trait BlackPiece extends Piece with PieceType {
  val color = 1
  val name = "Black" + pieceType
}

case object WhiteKing extends WhitePiece with KingPieceType 
class WhiteQueen extends WhitePiece with QueenPieceType 
class WhiteBishop extends WhitePiece with BishopPieceType
class WhiteKnight extends WhitePiece with KnightPieceType
class WhiteRook extends WhitePiece with RookPieceType
case object WhitePawn1 extends WhitePiece with PawnPieceType
case object WhitePawn2 extends WhitePiece with PawnPieceType
case object WhitePawn3 extends WhitePiece with PawnPieceType
case object WhitePawn4 extends WhitePiece with PawnPieceType
case object WhitePawn5 extends WhitePiece with PawnPieceType
case object WhitePawn6 extends WhitePiece with PawnPieceType
case object WhitePawn7 extends WhitePiece with PawnPieceType
case object WhitePawn8 extends WhitePiece with PawnPieceType

case object BlackKing extends BlackPiece with KingPieceType
class BlackQueen extends BlackPiece with QueenPieceType
class BlackBishop extends BlackPiece with BishopPieceType
class BlackKnight extends BlackPiece with KnightPieceType
class BlackRook extends BlackPiece with RookPieceType
case object BlackPawn1 extends BlackPiece with PawnPieceType
case object BlackPawn2 extends BlackPiece with PawnPieceType
case object BlackPawn3 extends BlackPiece with PawnPieceType
case object BlackPawn4 extends BlackPiece with PawnPieceType
case object BlackPawn5 extends BlackPiece with PawnPieceType
case object BlackPawn6 extends BlackPiece with PawnPieceType
case object BlackPawn7 extends BlackPiece with PawnPieceType
case object BlackPawn8 extends BlackPiece with PawnPieceType


object PieceRenderer {
  def apply(pieceOp: Option[Piece]): String = pieceOp match {
    case None => " "
    case Some(_: WhitePiece with KingPieceType)   => "\u2654"
    case Some(_: WhitePiece with QueenPieceType)  => "\u2655"
    case Some(_: WhitePiece with BishopPieceType) => "\u2656"
    case Some(_: WhitePiece with KnightPieceType) => "\u2657"
    case Some(_: WhitePiece with RookPieceType)   => "\u2658"
    case Some(_: WhitePiece with PawnPieceType)   => "\u2659"
    case Some(_: BlackPiece with KingPieceType)   => "\u265A"
    case Some(_: BlackPiece with QueenPieceType)  => "\u265B"
    case Some(_: BlackPiece with BishopPieceType) => "\u265C"
    case Some(_: BlackPiece with KnightPieceType) => "\u265D"
    case Some(_: BlackPiece with RookPieceType)   => "\u265E"
    case Some(_: BlackPiece with PawnPieceType)   => "\u265F"
  }
}

object Pieces {
  val whitePieces = new WhiteRook ::
    new WhiteKnight ::
    new WhiteBishop ::
    new WhiteQueen ::
    WhiteKing ::
    new WhiteBishop ::
    new WhiteKnight ::
    new WhiteRook ::
    WhitePawn1 ::
    WhitePawn2 ::
    WhitePawn3 ::
    WhitePawn4 ::
    WhitePawn5 ::
    WhitePawn6 ::
    WhitePawn7 ::
    WhitePawn8 :: Nil

  val whiteInit = whitePieces.zip(0 to 15)
  
  val blackPieces =
    BlackPawn1 ::
    BlackPawn2 ::
    BlackPawn3 ::
    BlackPawn4 ::
    BlackPawn5 ::
    BlackPawn6 ::
    BlackPawn7 ::
    BlackPawn8 ::
    new BlackRook ::
    new BlackKnight ::
    new BlackBishop ::
    new BlackQueen ::
    BlackKing ::
    new BlackBishop ::
    new BlackKnight ::
    new BlackRook ::
     Nil

  val blackInit = blackPieces.zip(48 to 63)
}
