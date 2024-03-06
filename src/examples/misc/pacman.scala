package examples.misc

import server.quickstart.play
import synth.base.{Constant, Synth}
import synth.simple.{Saw, Sine}

object pacman {
  def main(args: Array[String]): Unit = {

    val s = new Sine(2)
    val sound = 0.5 * (s + 1) * (new Sine(500)) + 0.5 * (1 - s) * (new Sine(600))

    play(sound)
  }
}
