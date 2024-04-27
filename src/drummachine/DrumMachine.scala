package drummachine

import sequence.visual.VisualSequencer
import server.quickstart.play
import synth.base.Synth
import util.conversions.dbl2conv

abstract class DrumMachine(tempoBPM: Double, filename: Option[String]=None) {

  val sequencer = new VisualSequencer(tempoBPM.bpm, filename=filename)

  play(patch)
  def patch: Synth

}
