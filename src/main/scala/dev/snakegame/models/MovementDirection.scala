package dev.snakegame.models

object MovementDirection {
  sealed trait MovementDirection
  case object Up            extends MovementDirection
  case object Down          extends MovementDirection
  case object LeftMovement  extends MovementDirection
  case object RightMovement extends MovementDirection
}
