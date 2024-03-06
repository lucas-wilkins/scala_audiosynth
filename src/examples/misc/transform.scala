package examples.misc

import filter.simple.Integrator
import server.quickstart.play
import synth.simple.Sine

object transform {
  def main(args: Array[String]): Unit = {

    /*
     This produces a cool effect, because the effect of the frequency change is effectively larger as time gets bigger
     */

    val angle = (2 * math.Pi) * new Integrator(1)
    val freq = 250 + 10 * new Sine(10)

    val output = (angle * freq).transformed(math.sin)

    play(output)
  }
}
