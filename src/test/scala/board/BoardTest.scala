package board

import org.scalatest.FunSuite
import pieces._

class BoardTest extends FunSuite {

  test("new board creation and clearing") {

    val b = new Board().initSetup()

    println(b.render)
    println("\n")

    println(b.clear())
    println("\n")

  }

  test("update a position") {

    val b = new Board().update(Position(3,3), new WhiteKnight)

    assert(b.pieceAt(3, 3).exists{case p: WhiteKnight => true})


  }

  test("clear a position") {

    val b = new Board().initSetup()

    assert(b.pieceAt(1,1).isDefined)

    assert(b.clear(Position(1,1)).pieceAt(1,1).isEmpty)

  }

  test("rank get a full rank") {

    val b = new Board().initSetup()

    val ls = b.rank(2)
    assert(ls.size == 8)
    assert(ls.forall(op => op.isDefined && op.exists(_.isInstanceOf[PawnPieceType]) && op.exists(_.color == 0)))

  }

  test("rank get a full rank (black)") {

    val b = new Board().initSetup()

    val ls = b.rank(7)
    assert(ls.size == 8)
    assert(ls.forall(op => op.isDefined && op.exists(_.isInstanceOf[PawnPieceType]) && op.exists(_.color == 1)))

  }

  test("file gets a full file") {

    val b = new Board().initSetup()

    val ls = b.file(2)
    assert(ls.size == 8)
    assert(ls.flatMap(_.map(_ => 1)).sum == 4) // non-empty cells are 4

    assert(ls(0).exists{case _: WhiteKnight => true})
    assert(ls(1).exists{case UnmovedWhitePawn2 => true})
    assert(ls(6).exists{case UnmovedBlackPawn2 => true})
    assert(ls(7).exists{case _: BlackKnight => true})

  }

  test("diagIteration returns the linear section when there are no steps to make") {

    val b = new Board().initSetup()

    assert(b.diagIteration(Position(1,1) :: Nil, 0, 0) == Position(1,1) :: Nil)
    assert(b.diagIteration(Position(2,3) :: Nil, 0, 0) == Position(2,3) :: Nil)
    assert(b.diagIteration(Position(1,1) :: Position(2,3) :: Nil, 0, 0) == Position(1,1) :: Position(2,3) :: Nil)

  }

  test("diagIteration returns the next position when (r,f) = (1,1)") {

    val b = new Board().initSetup()
    assert(b.diagIteration(Position(1,1) :: Nil, 1, 1) == Position(1,1) :: Position(2,2) :: Nil)

  }

  test("diagIteration returns the next 2 positions when (r,f) = (2,2)") {

    val b = new Board().initSetup()
    assert(b.diagIteration(Position(1,1) :: Nil, 2, 2) == Position(1,1) :: Position(2,2) :: Position(3,3) :: Nil)

  }

  test("diagIteration returns the next 2 positions when (r,f) = (2,2) and starting position is (6,6)") {

    val b = new Board().initSetup()
    assert(b.diagIteration(Position(6,6) :: Nil, 2, 2) == Position(6,6) :: Position(7,7) :: Position(8,8) :: Nil)

  }

  test("diagIteration returns the next 2 positions when (r,f) = (-2,-2) and starting position is (6,6)") {

    val b = new Board().initSetup()
    assert(b.diagIteration(Position(6,6) :: Nil, -2, -2) == Position(6,6) :: Position(5,5) :: Position(4,4) :: Nil)

  }

  test("diagIteration returns the next position when (r,f) = (1,1) and starting position is (7,7)") {

    val b = new Board().initSetup()
    assert(b.diagIteration(Position(7,7) :: Nil, 1, 1) == Position(7,7) :: Position(8,8) :: Nil)

  }

  test("diag gets a full diagonal - (1,1) -> (8,8)") {

    val b = new Board().initSetup()

    val ls = b.diag(Position(1,1), Position(8,8))
    assert(ls.size == 8)
    assert(ls(0).exists{case _: WhiteRook => true})
    assert(ls(1).exists{case UnmovedWhitePawn2 => true})
    assert(ls(6).exists{case UnmovedBlackPawn7 => true})
    assert(ls(7).exists{case _: BlackRook => true})
    assert(ls.flatMap(_.map(_ => 1)).sum == 4)

  }

  test("diag gets a full diagonal - (1,8) -> (8,1)") {

    val b = new Board().initSetup()

    val ls = b.diag(Position(1,8), Position(8,1))
    assert(ls.size == 8)
    assert(ls(0).exists{case _: WhiteRook => true})
    assert(ls(1).exists{case UnmovedWhitePawn7 => true})
    assert(ls(6).exists{case UnmovedBlackPawn2 => true})
    assert(ls(7).exists{case _: BlackRook => true})
    assert(ls.flatMap(_.map(_ => 1)).sum == 4)

  }

  test("diag gets a partial diagonal") {

    val b = new Board().initSetup()

    val ls = b.diag(Position(6,5), Position(8,7))
    assert(ls.size == 3)
    assert(ls.head.isEmpty)
    assert(ls(1).exists{case UnmovedBlackPawn6 => true})
    assert(ls(2).exists{case _: BlackKnight => true})


  }





}
