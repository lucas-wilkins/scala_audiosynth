package examples.sequencing

import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.envelope.LinearADSR
import synth.simple.{Sine, Square}
import util.conversions.int2conv
import util.scales.WellTempered

object sequencewithenv {
  def main(args: Array[String]): Unit = {
    val scale = WellTempered(440)

    val c3 = scale("C3")
    val eb3 = scale("Eb3")
    val g3 = scale("G3")
    val bb3 = scale("Bb3")

    val clock = new Square(4 * 180.bpm)

    // This sequences a single oscillator
    val continuousSeq = new LinearADSR(clock, 0.1, 0.1, 0.5, 0.1) *
      new Sine(new BasicSequencer(clock, c3, eb3, g3, bb3))

    // Play each one twice through, then swap
    play(0.2 * continuousSeq)

  }
}
