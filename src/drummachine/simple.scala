package drummachine

import filter.weird.Clip
import synth.base.Synth
import synth.simple.Sine
import sequence.visualutil.{TriggerHoldASDR, TriggerHoldToPositive, TriggerHoldToRect}
import synth.pitch.Portmanteau

object simple {
  def main(args: Array[String]): Unit = {
    new DrumMachine(200, Some("simple.seq")) {
      def patch: Synth = {
        val beep = new Sine(300) * new TriggerHoldASDR(sequencer.channel1, 0.01, 0.01, 0.5, 0.02)

        val clipper = new Clip(new Sine(100), (1-new TriggerHoldToPositive(sequencer.channel2)) * 10) * new Portmanteau(new TriggerHoldToRect(sequencer.channel2), 5)

        beep * sequencer.channel1vol + clipper * sequencer.channel2vol
      }
    }
  }
}
