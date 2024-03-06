package synth

import java.util.Random

import synth.base.AtomicSynth

object noise {

  class WhiteNoise(val seed: Long = 0) extends AtomicSynth {
    val rand: Random = new Random(seed)
    def calculate(): Double = 2 * rand.nextDouble() - 1
  }

}
