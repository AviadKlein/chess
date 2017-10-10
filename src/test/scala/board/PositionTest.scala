package board

import org.scalatest.FunSuite

class PositionTest extends FunSuite {

  test("creation") {

    println(Position(2,2))

  }

  test("index is calculated correctly") {

    assert(Position(1,1).index == 0)
    assert(Position(1,2).index == 1)
    assert(Position(1,3).index == 2)
    assert(Position(1,4).index == 3)
    assert(Position(1,5).index == 4)
    assert(Position(1,6).index == 5)
    assert(Position(1,7).index == 6)
    assert(Position(1,8).index == 7)

    assert(Position(4,1).index == 24)
    assert(Position(4,2).index == 25)
    assert(Position(4,3).index == 26)
    assert(Position(4,4).index == 27)

    assert(Position(8,5).index == 60)
    assert(Position(8,6).index == 61)
    assert(Position(8,7).index == 62)
    assert(Position(8,8).index == 63)

  }
  
  test("constructor from index") {

    assert(Position(0) == Position(1,1))
    assert(Position(1) == Position(1,2))
    assert(Position(2) == Position(1,3))
    assert(Position(3) == Position(1,4))
    assert(Position(4) == Position(1,5))
    assert(Position(5) == Position(1,6))
    assert(Position(6) == Position(1,7))
    assert(Position(7) == Position(1,8))

    assert(Position(24) == Position(4,1))
    assert(Position(25) == Position(4,2))
    assert(Position(26) == Position(4,3))
    assert(Position(27) == Position(4,4))

    assert(Position(60) == Position(8,5))
    assert(Position(61) == Position(8,6))
    assert(Position(62) == Position(8,7))
    assert(Position(63) == Position(8,8))
    
  }

  test("throws error when rank is larger than 8") {
    val exception = intercept[IllegalArgumentException](Position(9,1))
    assert(exception.getMessage.contains("Invalid position"))
  }

  test("throws error when rank is smaller than 1") {
    val exception = intercept[IllegalArgumentException](Position(0,1))
    assert(exception.getMessage.contains("Invalid position"))
  }

  test("throws error when file is larger than 8") {
    val exception = intercept[IllegalArgumentException](Position(1,9))
    assert(exception.getMessage.contains("Invalid position"))
  }

  test("throws error when file is smaller than 1") {
    val exception = intercept[IllegalArgumentException](Position(1,0))
    assert(exception.getMessage.contains("Invalid position"))
  }

  test("manhattan steps returns 0,0 when it's the same position") {
    val p1 = Position(2,2)
    val p2 = Position(2,2)
    assert((p1 manhattanSteps p2) == (0,0))
  }

  test("manhattan steps returns 0,1 when the position is 1 file to the right") {
    val p1 = Position(2,2)
    val p2 = Position(2,3)
    assert((p1 manhattanSteps p2) == (0,1))
  }

  test("manhattan steps returns 0,-1 when the position is 1 file to the left") {
    val p1 = Position(2,2)
    val p2 = Position(2,1)
    assert((p1 manhattanSteps p2) == (0,-1))
  }

  test("manhattan steps returns 1,0 when the position is 1 rank up") {
    val p1 = Position(2,2)
    val p2 = Position(3,2)
    assert((p1 manhattanSteps p2) == (1,0))
  }

  test("manhattan steps returns -1,0 when the position is 1 rank down") {
    val p1 = Position(2,2)
    val p2 = Position(1,2)
    assert((p1 manhattanSteps p2) == (-1,0))
  }

  test("manhattan steps returns 1,1 when the position is diagonally up and to the right") {
    val p1 = Position(2,2)
    val p2 = Position(3,3)
    assert((p1 manhattanSteps p2) == (1,1))
  }

  test("diagonalTo returns `true` when two Positions are diagonal to one another") {

    assert(Position(1,1) isDiagonalTo Position(5,5))
    assert(Position(2,3) isDiagonalTo Position(5,6))
    assert(Position(4,3) isDiagonalTo Position(6,1))
    assert(Position(8,8) isDiagonalTo Position(1,1))
    assert(Position(8,1) isDiagonalTo Position(1,8))

  }

  test("diagonalTo returns `false` when two Positions are not diagonal to one another") {

    assert(!(Position(1,1) isDiagonalTo Position(4,5)))
    assert(!(Position(2,3) isDiagonalTo Position(4,6)))
    assert(!(Position(4,3) isDiagonalTo Position(2,2)))
    assert(!(Position(8,8) isDiagonalTo Position(7,1)))
    assert(!(Position(8,1) isDiagonalTo Position(5,8)))

  }

  test("diagonal step functions") {

    val p = Position(3,3)
    assert(p.diagRightUp == Position(4,4))
    assert(p.diagRightDown == Position(2,4))
    assert(p.diagLeftUp == Position(4,2))
    assert(p.diagLeftDown == Position(2,2))

  }
  
  

}
