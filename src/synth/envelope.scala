package synth

import synth.base.Synth
import util.conversions.dbl2conv

object envelope {

  class LinearADSR(val trigger: Synth, val attack: Synth, val decay: Synth, val sustain: Synth, val release: Synth) extends Synth {

    private var lastTriggerValue: Double = 0.0

    private var state: Int = 4
    private var value: Double = 0

    def calculate(): Double = {
      val triggerValue = trigger.outputValue

      state match {
        case 1 => {
          value += dt / attack.outputValue.enforcedPositive
          if (value > 1) {
            value = 1
            state = 2
          }
        }

        case 2 => {
          value -= dt / decay.outputValue.enforcedPositive
          val sus = sustain.outputValue.enforcedZeroToOne
          if (value < sus) {
            value = sus
            state = 3
          }
        }

        case 3 => // value = sustain.read

        case 4 => {
          value -= dt / release.outputValue.enforcedPositive
          if (value < 0) {
            value = 0
          }
        }
        case _ => // Do nothing
      }

      // Rising edge
      if ((lastTriggerValue <= 0) && (triggerValue > 0)) {
        //println("Rising trigger")
        state = 1
        value = 0
      }

      // Falling edge
      if ((lastTriggerValue >= 0) && (triggerValue < 0)) {
        //println("Falling trigger")
        state = 4
        value = sustain.outputValue
      }

      lastTriggerValue = triggerValue

      value
    }

    val children: List[Synth] = trigger :: attack :: decay :: sustain :: release :: Nil


  }

  class ADSR(val trigger: Synth, val attack: Synth, val decay: Synth, val sustain: Synth, val release: Synth) extends Synth {

    private var lastTriggerValue: Double = -1

    private var state: Int = -1
    private var value: Double = 0

    private val ln2 = math.log(2)

    def calculate(): Double = {
      val triggerValue = trigger.outputValue

      state match {
        case 1 => {
          value += dt / attack.outputValue.enforcedPositive
          if (value > 1) {
            value = 1
            state = 2
          }
        }

        case 2 => {
          value -= (ln2 * dt / decay.outputValue.enforcedPositive ) * (outputValue - sustain.outputValue.enforcedPositive)
        }

        case 3 => {
          value -= (ln2 * dt / decay.outputValue.enforcedPositive ) * outputValue
          if (value < 0) {
            value = 0
          }
        }
        case _ => // Do nothing
      }

      // Rising edge
      if ((lastTriggerValue <= 0) && (triggerValue > 0)) {
        state = 1
        value = 0
      }

      // Falling edge
      if ((lastTriggerValue >= 0) && (triggerValue < 0)) {
        state = 3
        value = sustain.outputValue
      }

      lastTriggerValue = triggerValue

      value
    }


    val children: List[Synth] = trigger :: attack :: decay :: sustain :: release :: Nil


  }

}
