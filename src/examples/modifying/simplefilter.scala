package examples.modifying

import filter.simple.{HighPass, LowPass}
import server.quickstart.play
import synth.noise.WhiteNoise
import synth.simple.{Saw, Sine}

object simplefilter {

  object lowpass {
    def main(args: Array[String]): Unit = {
      val noise = new WhiteNoise()
      val cutoff = 300 + 300 * new Saw(0.5)

      val filtered = new LowPass(noise, cutoff)

      play(filtered)

    }
  }

  object highpass {
    def main(args: Array[String]): Unit = {
      val noise = new WhiteNoise()
      val cutoff = 10000 + 9999 * new Sine(0.5)

      val filtered = new HighPass(noise, cutoff)

      play(filtered)

    }
  }


  def main(args: Array[String]): Unit = {
    highpass.main(args)
  }
}
