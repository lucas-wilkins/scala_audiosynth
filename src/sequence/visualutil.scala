package sequence

import synth.base.Synth

object visualutil {

  /**
   * Map a triggerHoldInput on [-1, 1] to [0, 1]
   *
   *  1 ______________
   *       |\
   *       | \
   *       |  \
   *  0 ___|___\_______
   *       |    \
   *       |     \
   *       |      \
   * -1 ___|_______\____
   *
   *   Goes to
   *
   *   1 ________________
   *        |\
   *        |  \
   *        |    \
   *   0 ___|______\_____
   */
  class TriggerHoldToPositive(triggerHoldInput: Synth) extends Synth {

    override def children: List[Synth] = triggerHoldInput :: Nil

    override def calculate(): Double = {

      0.5*(1 + triggerHoldInput.outputValue)
    }
  }

  /** 1 if sound is playing, 0 otherwise */
  class TriggerHoldToRect(triggerHoldInput: Synth) extends Synth {

    override def children: List[Synth] = triggerHoldInput :: Nil

    override def calculate(): Double = {
      if (triggerHoldInput.outputValue <= -1) {
        0
      } else {
        1
      }

    }
  }


  /** ASDR controlled by a saw-tooth like input
   *
   * The parameter specification is like a standard ASDR
   *
   * */
  class TriggerHoldASDR(triggerHoldInput: Synth, attackTime: Synth, decayTimeConstant: Synth, sustainIntensity: Synth, releaseTime: Synth) extends Synth {

    override def children: List[Synth] = triggerHoldInput :: attackTime :: decayTimeConstant :: sustainIntensity :: releaseTime :: Nil

    /*
     * States:
     *   0: Off / Release
     *   1: Rise
     *   2: Fall
     *   3: Sustain
     *
     */

    var state: Int = 0

    var output: Double = 0.0

    var lastInput: Double = 0.0
    override def calculate(): Double = {
      val input = triggerHoldInput.outputValue

      // Check for sound off state
      if (input == -1) {
        state = 0
      }

      // Check for zero crossing
      if ((input > 0) && (lastInput <= 0)) {
        state = 1
      }

      lastInput = input

      // Main sequence

      state match {

        case 0 => // Off/Release

          output -= dt / releaseTime.outputValue

          if (output < 0) {
            output = 0
          }


        case 1 => // Attack

          output += dt / attackTime.outputValue

          if (output > 1) {
            output = 1
            state = 2
          }

        case 2 => // Decay

          output -= dt / decayTimeConstant.outputValue

          if (output < sustainIntensity.outputValue) {
            output = sustainIntensity.outputValue
            state = 3
          }

        case 3 => // Sustain
          output = sustainIntensity.outputValue

      }

      output

    }
  }


  /** ASDR controlled by a saw-tooth like input
   *
   * The parameter specification is like a standard ASDR
   *
   * */
  class TriggerASDR(triggerHoldInput: Synth, attackTime: Synth, decayTimeConstant: Synth, sustainIntensity: Synth, sustainTime: Synth, releaseTime: Synth) extends Synth {

    override def children: List[Synth] = triggerHoldInput :: attackTime :: decayTimeConstant :: sustainIntensity :: sustainTime :: releaseTime :: Nil

    /*
     * States:
     *   0: Off / Release
     *   1: Rise
     *   2: Fall
     *   3: Sustain
     *
     */

    var state: Int = 0
    var output: Double = 0.0
    var lastInput: Double = 0.0
    var sustainCounter: Double = 0.0
    override def calculate(): Double = {
      val input = triggerHoldInput.outputValue

      // Check for sound off state
      if (input == -1) {
        state = 0
      }

      // Check for zero crossing
      if ((input > 0) && (lastInput <= 0)) {
        state = 1
      }

      lastInput = input

      // Main sequence

      state match {

        case 0 => // Off/Release

          output -= dt / releaseTime.outputValue

          if (output < 0) {
            output = 0
          }


        case 1 => // Attack

          output += dt / attackTime.outputValue

          if (output > 1) {
            output = 1
            state = 2
          }

        case 2 => // Decay

          output -= dt / decayTimeConstant.outputValue

          if (output < sustainIntensity.outputValue) {
            output = sustainIntensity.outputValue
            sustainCounter = 1.0
            state = 3
          }

        case 3 => // Sustain
          sustainCounter -= dt / sustainTime.outputValue
          output = sustainIntensity.outputValue
          if (sustainCounter <= 0) {
            state = 0
          }

      }

      output

    }
  }

}
