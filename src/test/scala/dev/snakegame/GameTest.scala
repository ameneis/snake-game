package dev.snakegame

import dev.snakegame.engine.Game
import dev.snakegame.exceptions.InvalidGameStateException
import dev.snakegame.models.StepCommand.{DownCommand, LeftCommand, RightCommand, UpCommand}
import dev.snakegame.models.{Board, BoardConfig, RandomGenerator}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GameTest extends AnyFlatSpec with Matchers with GivenWhenThen {
  implicit val generator: RandomGenerator = (_: Int, _: Int) => 0

  "Game" should "end when snake catching its own tail" in {
    Given("A 5x5 game with initial setup")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords                    = Some((2, 1))
    val game                              = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving the snake into its own tail")
    val gameJustBeforeFail = game
      .continue()
      .performStep(RightCommand)
      .performStep(DownCommand)

    Then("The game should fail as soon as the snake touches its tail")
    assertThrows[InvalidGameStateException] {
      gameJustBeforeFail.performStep(LeftCommand)
    }
  }

  "Snake" should "continue upwards" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords                    = Some((0, 0))
    val game                              = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving just forward without any turns")
    val gameAfterMoves = game
      .continue()
      .continue()

    Then("The board should be correct")
    val expectedResult =
      """@ . o . .
        |. . x . .
        |. . x . .
        |. . . . .
        |. . . . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "continue moving left if the snake moves horizontally" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving left and then right")
    val gameAfterMoves = game
      .performStep(LeftCommand)
      .performStep(RightCommand)

    Then("The snake should move only left")
    val expectedResult =
      """@ . . . .
        |. . . . .
        |o x x . .
        |. . . . .
        |. . . . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "continue moving right if the snake moves horizontally" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving right and then left")
    val gameAfterMoves = game
      .performStep(RightCommand)
      .performStep(LeftCommand)

    Then("The snake should move only right")
    val expectedResult =
      """@ . . . .
        |. . . . .
        |. . x x o
        |. . . . .
        |. . . . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "continue moving up if the snake moves vertically" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving up and then down")
    val gameAfterMoves = game
      .performStep(UpCommand)
      .performStep(DownCommand)

    Then("The snake should move only up")
    val expectedResult =
      """@ . o . .
        |. . x . .
        |. . x . .
        |. . . . .
        |. . . . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Game" should "combine turning and going up properly" in {
    Given("A 7x7 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(7, 7).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving up, right, up and left")
    val gameAfterMoves = game
      .performStep(UpCommand)
      .performStep(RightCommand)
      .performStep(UpCommand)
      .performStep(LeftCommand)

    Then("The snake should move only up")
    val expectedResult =
      """@ . . . . . .
        |. . . . . . .
        |. . . o x . .
        |. . . . x . .
        |. . . . . . .
        |. . . . . . .
        |. . . . . . .
        |""".stripMargin
    println(gameAfterMoves.toString)
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "appear on the other side if snake reaches the edge horizontally" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving right and reaching the edge")
    val gameAfterMoves = game
      .performStep(RightCommand)
      .continue()
      .continue()

    Then("The snake should appear on the other side")
    val expectedResult =
      """@ . . . .
        |. . . . .
        |o . . x x
        |. . . . .
        |. . . . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "appear on the other side if snake reaches the edge vertically" in {
    Given("A 5x5 game with initial setup and initial food pellet not on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((0, 0))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving right and reaching the edge")
    val gameAfterMoves = game
      .continue()
      .continue()
      .continue()

    Then("The snake should appear on the other side")
    val expectedResult =
      """@ . x . .
        |. . x . .
        |. . . . .
        |. . . . .
        |. . o . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }

  "Snake" should "become longer if it eats the food pellet" in {
    Given("A 5x5 game with initial setup and initial food pellet on the way")
    implicit val boardConfig: BoardConfig = BoardConfig(5, 5).toOption.get
    val initFoodCoords = Some((2, 1))
    val game = Game(Board.generateStartBoard(initFoodCoords))

    When("Moving right and reaching the edge")
    val gameAfterMoves = game
      .continue()

    Then("The snake should be longer after eating food pellet")
    val expectedResult =
      """. . . . .
        |. . o . @
        |. . x . .
        |. . x . .
        |. . x . .
        |""".stripMargin
    gameAfterMoves.toString should be(expectedResult)
  }
}
