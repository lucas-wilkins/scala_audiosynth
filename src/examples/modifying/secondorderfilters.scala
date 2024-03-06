package examples.modifying

import filter.advanced.{SecondOrderBandPass, SecondOrderHighPass, SecondOrderLowPass}
import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.noise.WhiteNoise
import synth.simple.{Sine, Square}

object secondorderfilters {
  def main(args: Array[String]): Unit = {
    val noise = new WhiteNoise()

    val clock = new Square(0.5)

    val freq = 400 + 100*new Sine(1)

    val lp = new SecondOrderLowPass(noise, freq, 1.0)
    val bp = new SecondOrderBandPass(noise, freq, 0.3)
    val hp = new SecondOrderHighPass(noise, 10*freq, 0.05)
    val nothing = 0.0

    play(new BasicSequencer(clock, lp, bp, hp, nothing))
  }
}
