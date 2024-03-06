package examples.compound

import filter.advanced.{SecondOrderBandPass, SecondOrderLowPass}
import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.noise.WhiteNoise
import synth.simple.{Saw, Sine, Square, Triangle}
import util.scales.{WellTempered, octave}

object formantexperiment {
  def main(args: Array[String]): Unit = {

    val clock = new Square(4)

    val scale = WellTempered(440)

    val notes = new BasicSequencer(clock, scale("c3"), scale("e3"), scale("g3"), scale("c4"), scale("g3"), scale("e3"))
      .lowered(octave)

    val base = new Saw(notes)
    //val filtered = SecondOrderLowPass(base, 360, 0.1) + SecondOrderLowPass(base, 650, 0.1)
    val filtered = new SecondOrderBandPass(new SecondOrderBandPass(base, 360, 0.1), 650, 0.1)

    play(10*filtered)

  }
}
