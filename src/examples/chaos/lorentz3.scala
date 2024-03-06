package examples.chaos

import chaos.lorentz.typicalLorentz
import filter.advanced.DelayLine
import filter.simple.LowPass
import filter.weird.LogQuantise
import midi.mididevice.MidiController
import server.quickstart.play
import synth.envelope.LinearADSR
import synth.pitch.Portmanteau
import synth.recursion.Socket
import synth.simple.{Sine, Square}
import util.conversions.int2conv

import java.util.concurrent.Delayed

object lorentz3 {

  def main(args: Array[String]): Unit = {
    val controller = new MidiController("nanoKONTROL2", 1)

    val speed = 0.1 + 9.9*new Portmanteau(controller.slider(0, initialValue=0.5, showEvents = true), 1)
    val masterVolume = new Portmanteau(controller.knob(16, 0.5, showEvents = true), 10)

    val freqBaseX = 100 + 1000 * new Portmanteau(controller.slider(1, initialValue=0.15, showEvents = true), 1)
    val freqRangeX = 0.1 + 9.9 * new Portmanteau(controller.slider(2, initialValue=0.25, showEvents = true), 1)



    val lorentz = typicalLorentz(speed)

    val freqX = freqBaseX + (freqRangeX * freqBaseX) * lorentz.x_variable


    val out = masterVolume * new LowPass(new Sine(new LogQuantise(freqX, 2, 256)),1000*controller.slider(3, 0.5))

    play(out)
  }
}
