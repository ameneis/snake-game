package dev.snakegame

import dev.snakegame.TestUtils.makeRandomGenerator
import dev.snakegame.models.{Board, BoardConfig, RandomGenerator}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardTest extends AnyFlatSpec with Matchers {
  implicit val generator: RandomGenerator = makeRandomGenerator(0)
  "Sample board map" should "render correct string when created automatically without init food pellet" in {
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val sampleBoard                       = Board.generateStartBoard()
    val result                            = sampleBoard.toString
    val expectedResult =
      """. . . . .
        |. . . . @
        |. . o . .
        |. . x . .
        |. . x . .
        |""".stripMargin
    result should be(expectedResult)
  }

  "Sample board map" should "render correct string when created automatically with init food pellet" in {
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val sampleBoard = Board.generateStartBoard(Some((0, 0)))
    val result = sampleBoard.toString
    val expectedResult =
      """@ . . . .
        |. . . . .
        |. . o . .
        |. . x . .
        |. . x . .
        |""".stripMargin
    result should be(expectedResult)
  }
}
