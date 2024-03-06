package examples.generators

import server.quickstart.play
import synth.simple.Saw

object sawwave {


  def main(args: Array[String]): Unit = {
    play(0.1 * new Saw(440))
  }
}
