package examples.modifying

import filter.weird.Quantise
import server.quickstart.play
import synth.simple.{Saw, Sine}

object quantise {
  def main(args: Array[String]): Unit = {
    val input = new Sine(440)

    val levels = 1 + 15 * (1 + new Saw(0.4))
    val timestep = 0.0001 + 0.001 * (1 + new Saw(0.1))

    play(0.1 * new Quantise(input, levels, timestep))
  }
}
