package examples.misc

import filter.weird.LogQuantise
import server.quickstart.play
import synth.debug.Poll
import synth.simple.Sine

object quantisefreq {
  /**
   * Example of using LogQuantise to round frequencies to semitones
   *
   */
  def main(args: Array[String]): Unit = {
    val freq = new Poll(200*(1 + 0.5*(1+new Sine(0.5))), 0.1, text = "Frequency")

    val out = new Sine(new LogQuantise(freq, 1, 100))
//    val out = new Sine(freq)

    play(out)
  }
}
