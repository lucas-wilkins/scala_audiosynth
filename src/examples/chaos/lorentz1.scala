package examples.chaos

import chaos.lorentz.typicalLorentz
import midi.mididevice.MidiController
import server.quickstart.play
import synth.pitch.Portmanteau
import synth.simple.Sine

object lorentz1 {

  def main(args: Array[String]): Unit = {
    val controller = new MidiController("nanoKONTROL2", 1)

    val speed = 0.1 + 9.9*new Portmanteau(controller.slider(0, initialValue=0.5, showEvents = true), 1)
    val masterVolume = new Portmanteau(controller.knob(16, 0.5, showEvents = true), 10)

    val freqBaseX = 100 + 1000 * new Portmanteau(controller.slider(1, initialValue=0.15, showEvents = true), 1)
    val freqRangeX = 0.1 + 9.9 * new Portmanteau(controller.slider(2, initialValue=0.1, showEvents = true), 1)
    val volumeX = new Portmanteau(controller.knob(17, 1), 10)

    val freqBaseY = 100 + 1000 * new Portmanteau(controller.slider(3, initialValue=0.15, showEvents = true), 1)
    val freqRangeY = 0.1 + 9.9 * new Portmanteau(controller.slider(4, initialValue=0.1, showEvents = true), 1)
    val volumeY = new Portmanteau(controller.knob(19, 1), 10)

    val freqBaseZ = 100 + 1000 * new Portmanteau(controller.slider(5, initialValue=0.15, showEvents = true), 1)
    val freqRangeZ = 0.1 + 9.9 * new Portmanteau(controller.slider(6, initialValue=0.1, showEvents = true), 1)
    val volumeZ = new Portmanteau(controller.knob(21, 1), 10)


    val lorentz = typicalLorentz(speed)

    val freqX = freqBaseX + (freqRangeX * freqBaseX) * lorentz.x_variable
    val freqY = freqBaseY + (freqRangeY * freqBaseY) * lorentz.y_variable
    val freqZ = freqBaseZ + (freqRangeZ * freqBaseZ) * lorentz.z_variable



    val out = masterVolume*(
      volumeX * new Sine(freqX) +
        volumeY * new Sine(freqY) +
        volumeZ * new Sine(freqZ))/3

    play(out)
  }
}
