package drummachine

import synth.base.Synth
import synth.simple.Sine

object debug {
  def main(args: Array[String]): Unit = {
    new DrumMachine(200, Some("debug.seq")) {
      def patch: Synth = {
        new Sine(300 + 100*sequencer.channel1)
      }
    }
  }
}
