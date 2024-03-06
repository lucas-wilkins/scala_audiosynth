package examples.generators

import server.quickstart.play
import synth.simple.Sine

object basicsinewave {


  def main(args: Array[String]): Unit = {
    val s1 = 0.1 * new Sine(440)

    play(s1)
  }
}
