package examples.misc

import filter.simple.{Integrator, LowPass}
import server.quickstart.play
import synth.base.{Constant, Synth}
import synth.debug.Poll
import synth.pitch.Portmanteau
import synth.simple.{Saw, Sine, Square}

/**
 *  Shepard's tone, was hard to get right, the intervals being harmonic really helps, so does doing things slowly
 */
object shepard {
  def main(args: Array[String]): Unit = {
//    val n: Int = 9 // With 8 octaves, 9 is every octave, 17 is tritones
    val n: Int = 17

    val low: Double = 20 // Lowest frequency
    val octaves: Double = 8 // Number of octaves to span

    val period: Double = 60 // How long to go from low to high

    val down: Boolean = true // go down in pitch rather than up

    def createSynth(index: Int): Synth = {
      val modulationPhase = (if (down) -1 else 1) * 0.5 *  new Saw(1.0 / period, index.toDouble / n) // Between -0.5 and 0.5

      val amplitudeEnvelope = 1 - 4 * (modulationPhase ^ 2) // Between 0 and 1
      val log2freq = (1 + 2*modulationPhase) * (0.5 * octaves) // 0 to octaves
      val freq = low * (new Constant(2.0)^log2freq)

      val amplitudePowerCorrection = 100/freq // reduced for constant power

      new Sine(freq) * amplitudeEnvelope * amplitudePowerCorrection

    }

    val oscillators: Synth =
      (0 until n)
        .toList
        .map(createSynth)
        .fold(new Constant(0.0))(_ + _) / n

    play(oscillators)
  }
}
