package examples.sequencing

import sequence.vcotype.{BasicSequencer, MutatingSequencer}
import server.quickstart.play
import synth.base.dbl2synth
import synth.envelope.{ADSR, LinearADSR}
import synth.simple.{Saw, Sine, Square}
import util.conversions.int2conv
import util.scales._

object mutatingsequence {

  def main(args: Array[String]): Unit = {
    val scale = WellTempered(440)

    val c3 = scale("C3")
    val eb3 = c3 raised minorThird
    val g3 = c3 raised perfectFifth
    val bb3 = c3 raised minorSeventh
    val d4 = g3 raised perfectFifth

    val clock = new Square(4 * 180.bpm)

    val trill = 1 + 0.01 * new Sine(20)


    val continuousSeq =
      new Sine(new MutatingSequencer(
        clock,
        List(c3, eb3, g3, bb3),
        List(perfectFifth, majorThird, perfectFifth, perfectFourth).map(_.multiplier)))


    val baseMovingSeq = new ADSR(clock, 0.1, 0.1, 0.2, 0.1) *
      new Saw(new MutatingSequencer(
        clock,
        new BasicSequencer(clock, c3 * trill, eb3 * trill, g3 * trill, bb3 * trill, d4 * trill, bb3 * trill, g3 * trill, eb3 * trill) :: Nil,
        List(minorThird).map(_.multiplier),
        mutationFrequency = 2))


    //play( 0.5 * continuousSeq )
    play(0.5 * baseMovingSeq)

  }
}
