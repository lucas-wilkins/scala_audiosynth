package examples.generators

import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.simple.{MultiSine, NormalisedMultiSine, Saw, Sine, Square}

object multisine {
  def main(args: Array[String]): Unit = {
    val freq = 400

    val fundamental = new NormalisedMultiSine(freq)
    val oneHarmonic = new NormalisedMultiSine(freq, (0.7, 0))
    val twoHarmonics = new NormalisedMultiSine(freq, (0.7, 0), (0.5, 0))
    val threeHarmonics = new NormalisedMultiSine(freq, (0.7, 0), (0.5, 0), (0.35, 0))

    val clock = new Square(1)

    play(new BasicSequencer(clock, fundamental, oneHarmonic, twoHarmonics, threeHarmonics))
  }
}
