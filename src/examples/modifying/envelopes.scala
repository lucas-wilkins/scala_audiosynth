package examples.modifying

import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.envelope.{ADSR, LinearADSR}
import synth.simple.{Sine, Square}

object envelopes {
  def main(args: Array[String]): Unit = {
    val clock = new Square(1)
    val base = new Sine(250)

    val slowClock = clock.double.double

    val linear = new LinearADSR(clock, 0.1, 0.1, 0.5, 0.4) * base
    val log = new ADSR(clock, 0.1, 0.1, 0.5, 0.4) * base

    val alternating = new BasicSequencer(slowClock, linear, log)

    play(alternating)
  }
}
