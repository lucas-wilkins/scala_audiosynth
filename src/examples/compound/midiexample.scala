package examples.compound

import filter.advanced.{DelayLine, SecondOrderBandPass, SecondOrderHighPass, SecondOrderLowPass}
import filter.simple.LowPass
import filter.weird.Clip
import midi.mididevice.MidiController
import sequence.discrete.HoldingSequence
import sequence.vcotype.BasicSequencer
import server.quickstart.play
import synth.debug.PrintTriggerTime
import synth.envelope.{ADSR, LinearADSR}
import synth.noise.WhiteNoise
import synth.pluck.SoftPlucker
import synth.recursion.Socket
import synth.simple.{Sine, Square}
import util.conversions.int2conv
import util.scales._

object midiexample {
  def main(args: Array[String]): Unit = {
    val controller = new MidiController("nanoKONTROL2", 1)

    val clock = new Square(4 * 180.bpm) // Sixteenth notes
    //val clock = new PrintTriggerTime(clockBase)

    val volume = controller.slider(0, 1)
    val bassVolume = controller.slider(1, 0.5)
    val bassTrigger = controller.clockedToggle(64, clock, 32, initialValue = 0)

    val cutoff = controller.knob(16, initialValue = 1)
    val resonance = controller.knob(17, initialValue = 0.5)

    val hatTrigger =   new HoldingSequence(clock, "x-x-x-x-x-x-x-x-")
    val snareTrigger = new HoldingSequence(clock, "----x-------x---")
    val kickTrigger =  new HoldingSequence(clock, "x---------x-----")

    val kick = new SoftPlucker(kickTrigger, 45 + 0 * new Sine(1) + new Sine(10), 0.1, 0.001)

    val snareBase = new SecondOrderBandPass(new WhiteNoise(), frequency = 350 + 20 * new Sine(1), damping = 0.5)
    val snare = new LinearADSR(snareTrigger, 0.005, 0.15, 0, 0.1) * snareBase

    val hatBase = new SecondOrderHighPass(new WhiteNoise(), 8000, 0.1)
    val hat = new LinearADSR(hatTrigger, 0.02, 0.1, 0, 0) * hatBase

    val drums = kick + 2 * snare + 2 * hat


    val a :: b :: c :: d :: Nil = List[Interval](unison, minorSecond, majorSecond, minorThird).map(80.0 * _.multiplier)


    val melody = new BasicSequencer(
      clock.double,
      a, a, a, a,
      d, d, d, d,
      d, c, b, b,
      b, b, b, b)
                                                        //  1---2---3---4---
    val onValues = new HoldingSequence(clock.double, "x---x x x--xx-x-")

    val bass = new ADSR(onValues, 0.1, 0.1, 1, 0.1) * new Clip(new Sine(melody + new Sine(10)), 1 + new Sine(1))

    //play(bass)


    val socket = new Socket()
    val reverb = new DelayLine(socket + drums + 0.5 * bassVolume * bassTrigger * bass, 0.1)

    val feedback = 0.1 * reverb
    socket connect feedback



    val all = volume * reverb

    // Controls are slider 1 for volume, R1 for bassline, knob 1 for filter

    play(all)
  }
}
