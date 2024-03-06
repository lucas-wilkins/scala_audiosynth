package examples.midi

import filter.weird.Clip
import midi.mididevice.MidiController
import server.quickstart.play
import synth.pitch.{PIDController, Portmanteau}
import synth.simple.Sine

object smoothing {
  def main(args: Array[String]): Unit = {
    val controller = new MidiController("nanoKONTROL2", 1)
    val p = controller.knob(16, 0.1, showEvents = true)
    val i = controller.knob(17, showEvents = true)
    val d = controller.knob(18, showEvents = true)

    val freq = 100 + 400* new PIDController(controller.knob(0), p, i, d)

    val wave = new Sine(freq)

    play(0.3*wave)
  }
}
