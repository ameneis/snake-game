package dev.snakegame.models

import dev.snakegame.models.Board.Coordinates

/**
 * Represents possible board elements along with its visual representation
 * and initial board placement
 */
object BoardElement {
  sealed trait BoardElement {
    def toAsciiChar: String
  }
  case object SnakeHead extends BoardElement {
    def toAsciiChar: String = "o"
    def initCoordinates(
      implicit config: BoardConfig
    ): Coordinates =
      (config.middlePosition, config.height - 3)
  }
  case object SnakeBody extends BoardElement {
    def toAsciiChar: String = "x"
    def initCoordinates(
      implicit config: BoardConfig
    ): List[Coordinates] =
      List((config.middlePosition, config.height - 2), (config.middlePosition, config.height - 1))
  }
  case object EmptySpace extends BoardElement {
    def toAsciiChar: String = "."
  }
  case object FoodPellet extends BoardElement {
    def toAsciiChar: String = "@"
  }
}
