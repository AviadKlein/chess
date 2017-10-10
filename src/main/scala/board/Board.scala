package board

import pieces._

/**
  * The board is represented by a 64 long array of optional pieces
  * position 0 in the array represents A1 (left most bottom)
  * position 1 represents B1 (next file, same rank)
  * position 63 represents H8 ... and so on
  */
class Board(val board: Seq[Option[Piece]] = Seq.fill[Option[Piece]](64)(None)) {
  require(board.size == 64, s"board size must be 64, got ${board.size}")

  import Board._

  final private lazy val indices = board.indices

  lazy private val piecesToPositions: Map[Piece, Position] = board.zipWithIndex.flatMap{
    case (None, _) => None
    case (Some(piece), i) => Some(piece -> Position(i))
  }.toMap

  /**
    * We update a single position with an optional piece
    */
  private def update(position: Position, pieceOp: Option[Piece]): Board = new Board(board.updated(position.index, pieceOp))
  private def update(index: Int, pieceOp: Option[Piece]): Board = update(Position(index), pieceOp)
  def update(position: Position, piece: Piece): Board = update(position, Some(piece))
  def clear(position: Position): Board = update(position, None)


  /**
    * we clear the board from all pieces
    */
  def clear(): Unit = new Board()

  /**
    * We place the pieces in their starting positions
    */
  def initSetup(): Board =
    (Pieces.whiteInit ++ Pieces.blackInit).foldLeft(new Board()){case (b, (piece, i)) => b.update(i, Some(piece))}

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
  def pieceAt(position: Position): Option[Piece] = board(position.index)
  def pieceAt(r: Int, f: Int): Option[Piece] = board(Position(r,f).index)

  /**
    * We return the full rank/file by it's index
    */
  def rank(i: Int): LinearSection = {
    require(i >= 1 && i <= 8, f"invalid rank, got $i")
    (0 to 7).map(file => board(file + (i-1)*8))
  }

  def rank(p1: Position, p2: Position): LinearSection = {
    require(p1 isInRankOf p2, f"positions's rank must be the same, got ($p1, $p2)")
    (p1.file to p2.file).map(f => pieceAt(p1.rank, f))
  }

  def file(p1: Position, p2: Position): LinearSection = {
    require(p1 isInFileOf p2, f"positions's file must be the same, got ($p1, $p2)")
    (p1.rank to p2.rank).map(r => pieceAt(r, p1.file))
  }

  def file(i: Int): LinearSection = {
    require(i >= 1 && i <= 8, f"invalid file, got $i")
    (0 to 7).map(rank => board((i-1) + 8*rank))
  }

  def rankOrFile(p1: Position, p2: Position): LinearSection = {
    require(p1 isInRankOrFileOf p2, f"positions's file or rank must be the same, got ($p1, $p2)")
    if(p1 isInFileOf p2) file(p1,p2) else rank(p1,p2)
  }

  /** rank, file or diagonal - for queens */
  def rfd(p1: Position, p2: Position): LinearSection = {
    require(p1 isLinearFrom p2, f"positions must be linear to one another, got ($p1, $p2)")
    if(p1 isInRankOrFileOf p2) rankOrFile(p1, p2) else diag(p1,p2)
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
    require(p1 isDiagonalTo p2, s"positions are not diagonal to one another, got ($p1, $p2)")
    val (r,f) = p1 manhattanSteps p2
    diagIteration(p1 :: Nil,r,f).map(pieceAt)
  }

  def render: String =
    (1 to 8).reverse.map(i => rank(i).map(op => PieceRenderer(op)).mkString("|","|","|")).mkString("\n")

  def move(p1: Position, p2: Position): Board =
    pieceAt(p1) -> pieceAt(p2) match {
      case (None, _) => throw new IllegalArgumentException("no piece at source position : $p1")
      case (Some(p), None) => ???
    }

  def isLegalMove(piece: Piece, source: Position, target: Position, targetPiece: Option[Piece]): Boolean =
    piece match {
        // Easy peezy
      case _: RookPieceType   => (source isInRankOrFileOf target) & rankOrFile(source,target).isMiddleEmpty
      case _: BishopPieceType => (source isDiagonalTo target) & diag(source, target).isMiddleEmpty
      case _: KnightPieceType => source isKnightStepFrom target
      case _: QueenPieceType  => (source isLinearFrom target) & rfd(source, target).isMiddleEmpty
        // Kings
      case _: KingPieceType   => (source isLinearFrom target) & (source linearDistanceFrom target) == 1 & ???
        // Pawns
      case _: WhitePawn if (source directionTo target) != 1 => false
      case _: BlackPawn if (source directionTo target) != -1 => false
      case _: WhitePawn if source isDiagonalTo target => targetPiece.isDefined & (source linearDistanceFrom target) == 1
      case _: BlackPawn if source isDiagonalTo target => targetPiece.isDefined & (source linearDistanceFrom target) == 1
      case _: PawnPieceType with Unmoved => (source isInFileOf target) & (source linearDistanceFrom target) <= 2
      case _: PawnPieceType with Moved => (source isInFileOf target) & (source linearDistanceFrom target) == 1
    }

}

object Board {
  /**
    * a linear section is a sequence of adjoining [[Position]]s in either a File, Rank or Diagonal
    */
  type LinearSection = Seq[Option[Piece]]

  implicit class LinearSectionOps(val linearSection: LinearSection) extends AnyVal {

    def isEmpty: Boolean = linearSection.forall(_.isEmpty)

    /**
      * Return the sequence removed of head and tail, returns empty else
      */
    def middle: LinearSection = linearSection match {
      case Nil => Nil
      case _ :: Nil => Nil
      case _ :: _ :: Nil => Nil
      case _ +: mid :+ _ => mid
    }

    def isMiddleEmpty: Boolean = middle.isEmpty

  }
}

case class Position(rank: Int, file: Int) {
  require(rank >= 1 && rank <= 8, s"Invalid position, got ($rank, $file)")
  require(file >= 1 && file <= 8, s"Invalid position, got ($rank, $file)")

  lazy val index: Int = (rank-1)*8 + file - 1

  lazy val color: Byte = (1 - (index % 2)).toByte

  def manhattanSteps(to: Position): (Int, Int) =
    (to.rank - this.rank, to.file - this.file)

  def isDiagonalTo(that: Position): Boolean =
    math.abs(this.rank - that.rank) == math.abs(this.file - that.file)

  def diagonalDistanceTo(that: Position): Int = isDiagonalTo(that) match {
    case true => math.abs(this.rank - that.rank)
    case false => throw new IllegalArgumentException(f"target position is not diagonal to source, source: $this target: $that")
  }

  /**
    * returns 1 for white's attacking direction, -1 for black, 0 for same rank
    */
  def directionTo(that: Position): Int = math.signum(that.file - file)

  def isInRankOf(that: Position): Boolean = this.rank == that.rank
  def isInFileOf(that: Position): Boolean = this.file == that.file
  def isInRankOrFileOf(that: Position): Boolean = isInRankOf(that) || isInFileOf(that)
  def isKnightStepFrom(that: Position): Boolean = {
    val r,f = math.abs(this.file - that.file) -> math.abs(this.rank - that.rank)
    (r,f) == (2,1) || (r,f) == (1,2)
  }
  def isLinearFrom(that: Position): Boolean = isInRankOrFileOf(that) || isDiagonalTo(that)

  def linearDistanceFrom(that: Position): Int = isLinearFrom(that) match {
    case true =>
      val (r,f) = manhattanSteps(that)
      math.max(math.abs(r),math.abs(f))
    case false => throw new IllegalArgumentException(s"target Position is not linear to source, got $that")
  }


  def diagRightUp = Position(rank + 1, file + 1)
  def diagRightDown = Position(rank - 1, file + 1)
  def diagLeftUp = Position(rank + 1, file - 1)
  def diagLeftDown = Position(rank - 1, file - 1)

  override def toString: String = ('a' to 'h')(file-1).toString + rank
}

object Position {
  def apply(index: Int): Position = {
    require(index >= 0 && index <= 63, s"index must be between 0 and 63, got $index")
    Position(index / 8 + 1,index % 8 + 1)
  }
}