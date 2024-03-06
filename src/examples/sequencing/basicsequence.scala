package examples.sequencing

import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.simple.{Sine, Square}
import util.conversions.int2conv
import util.scales.WellTempered

object basicsequence {
  def main(args: Array[String]): Unit = {
    val scale = WellTempered(440)

    val c3 = scale("C3")
    val eb3 = scale("Eb3")
    val g3 = scale("G3")
    val bb3 = scale("Bb3")

    val root = new Sine(c3)
    val minorThird = new Sine(eb3)
    val fifth = new Sine(g3)
    val seventh = new Sine(bb3)

    val clock = new Square(360.bpm)

    // This switches between oscillators - makes a click which isn't very nice
    val switchSeq = new BasicSequencer(clock, root, minorThird, fifth, seventh)

    // This sequences a single oscillator
    val continuousSeq = new Sine(new BasicSequencer(clock, c3, eb3, g3, bb3))

    // Play each one twice through, then swap
    val compClock = new Square(45.bpm)
    val compSeq = new BasicSequencer(compClock, continuousSeq, switchSeq)

    play(0.1 * compSeq)

  }
}
