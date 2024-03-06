package examples.misc

import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.base.dbl2synth
import synth.simple.{Sine, Square}
import util.conversions.int2conv
import util.scales.{WellTempered, minorSeventh, minorThird, perfectFifth}

object comparescales {
  def main(args: Array[String]): Unit = {
    val scale = WellTempered(440)

    val c3 = scale("C3")
    val eb3 = scale("Eb3")
    val g3 = scale("G3")
    val bb3 = scale("Bb3")

    val clock = new Square(8 * 180.bpm)

    val root = new Sine(new BasicSequencer(clock, c3, c3))
    val third = new Sine(new BasicSequencer(clock, eb3, c3 raised minorThird))
    val fifth = new Sine(new BasicSequencer(clock, g3, c3 raised perfectFifth))
    val seventh = new Sine(new BasicSequencer(clock, bb3, c3 raised minorSeventh))

    val chord = root + third + fifth + seventh

    play(0.1 * chord)
  }
}
