package pieces

/**
  * A Piece inherits from [[PieceType]] and has a name that describes it
  * It has a color, either 0 - white or 1 black
  */
trait Piece extends PieceType {
  val name: String
  val color: Byte
}

trait WhitePiece extends Piece {
  val color = 0
  val name = "White " + pieceType
}

trait BlackPiece extends Piece with PieceType {
  val color = 1
  val name = "Black" + pieceType
}

trait WhitePawn extends WhitePiece with PawnPieceType {
  val direction: Int = 1
}

trait BlackPawn extends BlackPiece with PawnPieceType {
  val direction: Int = -1
}

trait EnPassant extends PawnPieceType

class WhiteKing extends WhitePiece with KingPieceType
class WhiteQueen extends WhitePiece with QueenPieceType 
class WhiteBishop extends WhitePiece with BishopPieceType
class WhiteKnight extends WhitePiece with KnightPieceType
class WhiteRook extends WhitePiece with RookPieceType
case object UnmovedWhitePawn1 extends WhitePawn with Unmoved
case object UnmovedWhitePawn2 extends WhitePawn with Unmoved
case object UnmovedWhitePawn3 extends WhitePawn with Unmoved
case object UnmovedWhitePawn4 extends WhitePawn with Unmoved
case object UnmovedWhitePawn5 extends WhitePawn with Unmoved
case object UnmovedWhitePawn6 extends WhitePawn with Unmoved
case object UnmovedWhitePawn7 extends WhitePawn with Unmoved
case object UnmovedWhitePawn8 extends WhitePawn with Unmoved
case object MovedWhitePawn1 extends WhitePawn with Moved
case object MovedWhitePawn2 extends WhitePawn with Moved
case object MovedWhitePawn3 extends WhitePawn with Moved
case object MovedWhitePawn4 extends WhitePawn with Moved
case object MovedWhitePawn5 extends WhitePawn with Moved
case object MovedWhitePawn6 extends WhitePawn with Moved
case object MovedWhitePawn7 extends WhitePawn with Moved
case object MovedWhitePawn8 extends WhitePawn with Moved

class BlackKing extends BlackPiece with KingPieceType
class BlackQueen extends BlackPiece with QueenPieceType
class BlackBishop extends BlackPiece with BishopPieceType
class BlackKnight extends BlackPiece with KnightPieceType
class BlackRook extends BlackPiece with RookPieceType
case object UnmovedBlackPawn1 extends BlackPawn with Unmoved
case object UnmovedBlackPawn2 extends BlackPawn with Unmoved
case object UnmovedBlackPawn3 extends BlackPawn with Unmoved
case object UnmovedBlackPawn4 extends BlackPawn with Unmoved
case object UnmovedBlackPawn5 extends BlackPawn with Unmoved
case object UnmovedBlackPawn6 extends BlackPawn with Unmoved
case object UnmovedBlackPawn7 extends BlackPawn with Unmoved
case object UnmovedBlackPawn8 extends BlackPawn with Unmoved
case object MovedBlackPawn1 extends BlackPawn with Moved
case object MovedBlackPawn2 extends BlackPawn with Moved
case object MovedBlackPawn3 extends BlackPawn with Moved
case object MovedBlackPawn4 extends BlackPawn with Moved
case object MovedBlackPawn5 extends BlackPawn with Moved
case object MovedBlackPawn6 extends BlackPawn with Moved
case object MovedBlackPawn7 extends BlackPawn with Moved
case object MovedBlackPawn8 extends BlackPawn with Moved


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
    case _ => throw new Exception("unknown piece") // should be unreachable
  }
}

object Pieces {
  val whitePieces = new WhiteRook with Unmoved ::
    new WhiteKnight ::
    new WhiteBishop ::
    new WhiteQueen ::
    new WhiteKing with Unmoved ::
    new WhiteBishop ::
    new WhiteKnight ::
    new WhiteRook  with Unmoved ::
    UnmovedWhitePawn1 ::
    UnmovedWhitePawn2 ::
    UnmovedWhitePawn3 ::
    UnmovedWhitePawn4 ::
    UnmovedWhitePawn5 ::
    UnmovedWhitePawn6 ::
    UnmovedWhitePawn7 ::
    UnmovedWhitePawn8 :: Nil

  val whiteInit = whitePieces.zip(0 to 15)

  val blackPieces =
    UnmovedBlackPawn1 ::
    UnmovedBlackPawn2 ::
    UnmovedBlackPawn3 ::
    UnmovedBlackPawn4 ::
    UnmovedBlackPawn5 ::
    UnmovedBlackPawn6 ::
    UnmovedBlackPawn7 ::
    UnmovedBlackPawn8 ::
    new BlackRook with Unmoved ::
    new BlackKnight ::
    new BlackBishop ::
    new BlackQueen ::
    new BlackKing with Unmoved ::
    new BlackBishop ::
    new BlackKnight ::
    new BlackRook with Unmoved ::
     Nil

  val blackInit = blackPieces.zip(48 to 63)
}
