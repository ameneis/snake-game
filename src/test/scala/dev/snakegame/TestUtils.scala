package dev.snakegame

import dev.snakegame.models.RandomGenerator

object TestUtils {
  def makeRandomGenerator(result: Int): RandomGenerator = (_: Int, _: Int) => result
}
