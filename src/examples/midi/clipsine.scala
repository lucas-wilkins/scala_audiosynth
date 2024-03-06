package examples.midi

import filter.weird.Clip
import midi.mididevice.MidiController
import server.quickstart.play
import synth.debug.Poll
import synth.pitch.Portmanteau
import synth.simple.Sine

object clipsine {
  def main(args: Array[String]): Unit = {
    val controller = new MidiController("nanoKONTROL2", 1)
    val portRate = new Poll(controller.slider(0), 1)

    val freq = 100 + 400*new Portmanteau(controller.knob(16), portRate)
    val clip = 10 * new Portmanteau(controller.knob(17), portRate)

    val wave = new Clip(new Sine(freq), clip)

    play(0.3*wave)
  }
}
