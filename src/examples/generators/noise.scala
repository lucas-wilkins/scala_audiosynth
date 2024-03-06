package examples.generators

import server.quickstart.play
import synth.noise.WhiteNoise

object noise {
  def main(args: Array[String]): Unit = {
    val white = new WhiteNoise()

    play(white)


  }
}
