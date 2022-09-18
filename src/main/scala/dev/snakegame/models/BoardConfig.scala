package dev.snakegame.models

/**
 * Stores information about width and height of the board.
 * Both width and height must be odd, equal to each other and greater than 4.
 */
case class BoardConfig(width: Int, height: Int) {
  val middlePosition: Int =
    Math.floor(width / 2).toInt
}

object BoardConfig {
  def apply(width: Int, height: Int): Either[String, BoardConfig] =
    if (width == height && width % 2 != 0 && width >= 5) {
      Right(new BoardConfig(width, height))
    } else {
      Left("Both width and height must be dividable by 3 and equal to each other.")
    }
}
