package examples.modifying

import filter.advanced.DelayLine
import server.quickstart.play
import synth.base.Synth
import synth.envelope.LinearADSR
import synth.recursion.Socket
import synth.simple.{PWM, Sine}

object echo {
  def main(args: Array[String]): Unit = {

    val clock = new PWM(1, duty = 0.05)
    val input = new LinearADSR(clock, 10, 10, 0.5, 10) * new Sine(500)

    val socket = new Socket()

    val delayed: Synth = new DelayLine(input + 0.9 * socket, 0.7345)

    socket connect delayed

    play(delayed)

  }
}
