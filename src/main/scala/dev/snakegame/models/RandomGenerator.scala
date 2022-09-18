package dev.snakegame.models

trait RandomGenerator {
  def between(start: Int, end: Int): Int
}
