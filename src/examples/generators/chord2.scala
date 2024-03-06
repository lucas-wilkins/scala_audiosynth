package examples.generators

import server.quickstart.play
import synth.simple.Sine
import util.scales.WellTempered

object chord2 {


  def main(args: Array[String]): Unit = {
    val scale = WellTempered(440)

    val root = new Sine(scale("C3"))
    val minorThird = new Sine(scale("Eb3"))
    val fifth = new Sine(scale("G3"))

    play(0.1 * (root + minorThird + fifth))

  }
}
