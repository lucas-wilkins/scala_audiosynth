package examples.sequencing

import filter.advanced.{SecondOrderBandPass, SecondOrderHighPass, SecondOrderLowPass}
import sequence.discrete.BasicDrumSequencer
import server.quickstart.play
import synth.envelope.LinearADSR
import synth.noise.WhiteNoise
import synth.pluck.SoftPlucker
import synth.simple.{Saw, Sine, Square}
import util.conversions.int2conv

object basicdrumseq {
  def main(args: Array[String]): Unit = {
    val clock = new Square(4 * 180.bpm)

    val kick = 1 * new SoftPlucker(clock, 90 + 5 *new  Sine(1), 0.1, 0.001)
    val snareBase = new SecondOrderBandPass(new WhiteNoise(), frequency = 350, damping = 0.5)
    val snare = 2 * new LinearADSR(clock, 0.02, 0.1, 0, 0.1) * snareBase

    val hatBase = new SecondOrderHighPass(new WhiteNoise(), 1000, 0.1)
    val hat = 2 * new LinearADSR(clock, 0.2, 0.1, 0, 0) * hatBase

    // The problem with this sequencer is that it cuts off the tail of the sound
    // after each beat, leading to clicking noises
    val seq = new BasicDrumSequencer(clock,
      hat   -> "x-x-x-x-x-x-x-x-",
      snare -> "----x-------x---",
      kick  -> "x---------x---x-"
    )

    play(seq)
    //play(hatBase)
  }
}
