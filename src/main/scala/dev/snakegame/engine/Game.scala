package dev.snakegame.engine

import dev.snakegame.exceptions.InvalidGameStateException
import dev.snakegame.models.{Board, RandomGenerator}
import dev.snakegame.models.Board._
import dev.snakegame.models.BoardElement._
import dev.snakegame.models.MovementDirection._
import dev.snakegame.models.StepCommand._

/** Main coordinator of the snake's movement, also handling important game events */
case class Game(board: Board, currentDirection: MovementDirection = Up)(
  implicit generator: RandomGenerator
) {
  /**
   * Continues the game on whatever direction the snake was.
   *
   * @return Game object with updated state
   */
  def continue(): Game =
    currentDirection match {
      case Up            => moveUp()
      case Down          => moveDown()
      case LeftMovement  => moveLeft()
      case RightMovement => moveRight()
    }

  /**
   * Reacts to user's input and potentially changes movement direction
   * of the snake.
   * If the snake was moving horizontally, it doesn't change the direction on left or right command.
   * If the snake was moving vertically, it doesn't change the direction on up or down command.
   * In all other combination, the snake changes its direction as if it was faced upfront to the screen
   * (it's not relative to itself, but rather to the screen)
   *
   * @param direction      direction command
   * @return Game object with updated state
   */
  def performStep(direction: StepCommand): Game =
    currentDirection match {
      case Up =>
        direction match {
          case LeftCommand  => moveLeft().copy(currentDirection = LeftMovement)
          case RightCommand => moveRight().copy(currentDirection = RightMovement)
          case UpCommand    => moveUp()
          case DownCommand  => moveUp()
        }
      case Down =>
        direction match {
          case LeftCommand  => moveLeft().copy(currentDirection = LeftMovement)
          case RightCommand => moveRight().copy(currentDirection = RightMovement)
          case UpCommand    => moveDown()
          case DownCommand  => moveDown()
        }
      case LeftMovement =>
        direction match {
          case LeftCommand  => moveLeft()
          case RightCommand => moveLeft()
          case UpCommand    => moveUp().copy(currentDirection = Up)
          case DownCommand  => moveDown().copy(currentDirection = Down)
        }
      case RightMovement =>
        direction match {
          case LeftCommand  => moveRight()
          case RightCommand => moveRight()
          case UpCommand    => moveUp().copy(currentDirection = Up)
          case DownCommand  => moveDown().copy(currentDirection = Down)
        }
    }

  private def moveUp(): Game = {
    val computeNextXHeadPosition: Coordinates => Int = coords => coords._1
    val computeNextYHeadPosition: Coordinates => Int = coords => {
      val (_, currentYHeadPosition) = coords
      if (currentYHeadPosition == 0) {
        board.config.height - 1
      } else {
        currentYHeadPosition - 1
      }
    }
    prepareNextGameStatus(computeNextXHeadPosition, computeNextYHeadPosition)
  }

  private def moveDown(): Game = {
    val computeNextXHeadPosition: Coordinates => Int = coords => coords._1
    val computeNextYHeadPosition: Coordinates => Int = coords => {
      val (_, currentYHeadPosition) = coords
      if (currentYHeadPosition == board.config.height - 1) {
        0
      } else {
        currentYHeadPosition + 1
      }
    }
    prepareNextGameStatus(computeNextXHeadPosition, computeNextYHeadPosition)
  }

  private def moveLeft(): Game = {
    val computeNextXHeadPosition: Coordinates => Int = coords => {
      val (currentXHeadPosition, _) = coords
      if (currentXHeadPosition == 0) {
        board.config.width - 1
      } else {
        currentXHeadPosition - 1
      }
    }
    val computeNextYHeadPosition: Coordinates => Int = coords => coords._2
    prepareNextGameStatus(computeNextXHeadPosition, computeNextYHeadPosition)
  }

  private def moveRight(): Game = {
    val computeNextXHeadPosition: Coordinates => Int = coords => {
      val (currentXHeadPosition, _) = coords
      if (currentXHeadPosition == board.config.width - 1) {
        0
      } else {
        currentXHeadPosition + 1
      }
    }
    val computeNextYHeadPosition: Coordinates => Int = coords => coords._2
    prepareNextGameStatus(computeNextXHeadPosition, computeNextYHeadPosition)
  }

  private def prepareNextGameStatus(
    computeNextXHeadPosition: Coordinates => Int,
    computeNextYHeadPosition: Coordinates => Int
  ): Game = {
    /* Handling Snake coordinates */
    val lastSnakeElementCoordinates = board.snakeCoordinates.last
    val headXPosition               = board.headCoordinates._1
    val headYPosition               = board.headCoordinates._2
    val nextHeadCoordinates =
      (computeNextXHeadPosition(headXPosition, headYPosition), computeNextYHeadPosition(headXPosition, headYPosition))
    val mapWithUpdatedSnake = board.boardMap
      .update(nextHeadCoordinates, SnakeHead)
      .update(board.headCoordinates, SnakeBody)

    /* Handling crash scenario */
    if (didPlayerLoose(nextHeadCoordinates)) {
      throw new InvalidGameStateException("You lost!")
    }

    /* Handling food eating scenario */
    val willEat = willEatFood(nextHeadCoordinates)
    val newSnakeCoordinates = if (willEat) {
      List(nextHeadCoordinates) ++ board.snakeCoordinates
    } else {
      List(nextHeadCoordinates) ++ board.snakeCoordinates.dropRight(1)
    }
    val updatedBoard: Board = if (willEat) {
      board.copy(boardMap = mapWithUpdatedSnake).placeFoodRandomly()
    } else {
      board.copy(boardMap = mapWithUpdatedSnake.update(lastSnakeElementCoordinates, EmptySpace))
    }

    this.copy(updatedBoard.copy(snakeCoordinates = newSnakeCoordinates))
  }

  private def willEatFood(nextHeadCoordinates: Coordinates): Boolean =
    board.boardMap(nextHeadCoordinates) == FoodPellet

  private def didPlayerLoose(nextHeadCoordinates: Coordinates) =
    board.boardMap(nextHeadCoordinates) == SnakeBody

  override def toString: String = board.toString
}
