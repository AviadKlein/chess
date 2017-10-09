package board

import org.scalatest.FunSuite
import pieces.WhiteKnight

class BoardTest extends FunSuite {

  test("new board creation and clearing") {

    val b = new Board
    b.restart() // mutable

    println(b.render)
    println("\n")

    b.clear()

    println(b.render)
    println("\n")

  }

  test("update a position") {

    val b = new Board

    b.update(Position(3,3), new WhiteKnight)

    println(b.render)
    println("\n")
  }

}
