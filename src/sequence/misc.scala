package sequence

import synth.base.{ModifyingSynth, Synth}

object misc {

  trait Doublable extends Synth {
    def double: PeriodDoubler = new PeriodDoubler(this)
  }

  class PeriodDoubler(val input: Synth) extends ModifyingSynth with Doublable {
    private var lastValue: Double = 0.0
    private var state: Double = -1
    override def calculate(): Double = {

      val thisValue = input.outputValue
      if ((lastValue <= 0.0) && (thisValue > 0.0)) {
        state = -state
      }

      lastValue = thisValue

      state

    }
  }
}
