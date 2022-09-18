package dev.snakegame

import dev.snakegame.engine.Game
import dev.snakegame.models.StepCommand._
import dev.snakegame.models.{Board, BoardConfig, RandomGenerator}

import scala.util.Random

object Main {
  implicit val generator: RandomGenerator = (start: Int, end: Int) => Random.between(start, end)
  implicit val boardConfig: BoardConfig   = BoardConfig(13, 13).toOption.get
  val game: Game                        = Game(Board.generateStartBoard())

  def main(args: Array[String]): Unit = {
    (1 until 100).foldLeft(game) { (game, _) =>
      Thread.sleep(1000)
      val gameAfterStep = Random.between(0, 5) match {
        case 0 => game.continue()
        case 1 => game.performStep(RightCommand)
        case 2 => game.performStep(LeftCommand)
        case 3 => game.performStep(UpCommand)
        case 4 => game.performStep(DownCommand)
      }
      println(gameAfterStep.toString)
      gameAfterStep
    }
  }
}
