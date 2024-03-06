package examples.generators

import server.quickstart.play
import synth.base.dbl2synth
import synth.simple.Sine
import util.scales.{WellTempered, minorSeventh, minorThird, perfectFifth}

object chord {


  def main(args: Array[String]): Unit = {

    val scale = WellTempered(440)
    val root: Double = scale("C3")

    val first = new Sine(root)
    val third = new Sine(root raised minorThird)
    val fifth = new Sine(root raised perfectFifth)
    val seventh = new Sine(root raised minorSeventh)

    play(0.1 * (first + third + fifth + seventh))

  }
}
