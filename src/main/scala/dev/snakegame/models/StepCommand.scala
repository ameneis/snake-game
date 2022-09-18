package dev.snakegame.models

object StepCommand {
  sealed trait StepCommand
  case object LeftCommand  extends StepCommand
  case object RightCommand extends StepCommand
  case object UpCommand    extends StepCommand
  case object DownCommand  extends StepCommand
}
