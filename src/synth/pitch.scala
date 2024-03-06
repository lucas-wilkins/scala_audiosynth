package synth

import synth.base.Synth
import util.conversions.dbl2conv

object pitch {
  class Portmanteau(input: Synth, slewRate: Synth) extends Synth {
    private var output: Double = 0.0

    override def calculate(): Double = {
      val target = input.outputValue
      val delta = dt * slewRate.outputValue

      if (output < target - delta) {
        output += delta
      } else if (output > target + delta) {
        output -= delta
      } else {
        output = target
      }

      output
    }

    override val children: List[Synth] = input :: slewRate :: Nil
  }

  class PIDController(input: Synth, p: Synth, i: Synth, d: Synth, initialValue: Double = 0.0) extends Synth {

    private var integral: Double = 0.0
    private var currentValue: Double = initialValue
    private var lastInput: Double = initialValue

    override def calculate(): Double = {
      val thisInput = input.outputValue
      val error = currentValue - thisInput

      val deltaT = dt

      val differential = (lastInput - thisInput)*sampleRate
      integral += error * deltaT
      integral = integral.clipMagnitudeOne

      currentValue -= p.outputValue * error * deltaT
      currentValue -= i.outputValue * integral * deltaT
      currentValue -= d.outputValue * differential * deltaT

      lastInput = thisInput

      currentValue
    }

    override val children: List[Synth] = input :: p :: i :: d :: Nil
  }

}
