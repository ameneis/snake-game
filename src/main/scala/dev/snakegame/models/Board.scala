package dev.snakegame.models

import dev.snakegame.models.Board._
import dev.snakegame.models.BoardElement._

import scala.collection.immutable.Queue

/**
 * Stores information about the game board and snake coordinates
 */
case class Board(boardMap: BoardMap, config: BoardConfig, snakeCoordinates: List[Coordinates])(
  implicit generator: RandomGenerator
) {
  val headCoordinates: Coordinates =
    boardMap.filter(_._2 == SnakeHead).head._1

  def placeFoodRandomly(): Board = {
    val availableCoordinates    = boardMap.removedAll(snakeCoordinates).keys.toList
    val chosenRandomCoordinates = availableCoordinates(generator.between(0, availableCoordinates.size - 1))
    this.copy(boardMap = boardMap.update(chosenRandomCoordinates, FoodPellet))
  }

  override def toString: String =
    (0 until config.height).map { row =>
      val rowString = (0 until config.width)
        .map { column =>
          boardMap((column, row)).toAsciiChar
        }
        .mkString(" ")
      s"$rowString\n"
    }.mkString
}

object Board {
  type Coordinates = (Int, Int)
  type BoardMap    = Map[Coordinates, BoardElement]

  implicit class BetterBoardMap(val bm: BoardMap) {
    def update(coordinates: (Int, Int), value: BoardElement): BoardMap =
      bm.updatedWith(coordinates)(_.map(_ => value))

    def update(coordinates: List[(Int, Int)], value: BoardElement): BoardMap =
      coordinates.foldLeft(bm)((accBm, coords) => accBm.update(coords, value))
  }

  def generateStartBoard(initialFoodCoordinates: Option[Coordinates] = None)(
    implicit config: BoardConfig,
    generator: RandomGenerator
  ): Board = {
    val mapWithSnake = emptyMap
      .update(SnakeHead.initCoordinates, SnakeHead)
      .update(SnakeBody.initCoordinates, SnakeBody)
      .update(SnakeHead.initCoordinates, SnakeHead)
    initialFoodCoordinates match {
      case Some(initFoodCoords) =>
        Board(
          mapWithSnake.update(initFoodCoords, FoodPellet),
          config,
          List(SnakeHead.initCoordinates) ++ SnakeBody.initCoordinates
        )
      case None =>
        Board(mapWithSnake, config, List(SnakeHead.initCoordinates) ++ SnakeBody.initCoordinates).placeFoodRandomly()
    }
  }

  private def emptyMap(
    implicit config: BoardConfig
  ): BoardMap =
    (for {
      row    <- (0 until config.height)
      column <- (0 until config.width)
    } yield ((row, column), EmptySpace))
      .to(Queue)
      .toMap
}
