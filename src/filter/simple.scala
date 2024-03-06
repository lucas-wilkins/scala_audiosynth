package filter

import synth.base.{ModifyingSynth, Synth}
import util.conversions.dbl2conv

object simple {

  private val twoPi: Double = 2*math.Pi

  class HighPass(val input: Synth, val cutoff: Synth) extends Synth {
    private var lastInput = 0.0

    override def calculate(): Double = {
      val freq = twoPi * cutoff.outputValue.enforcedPositive // Maybe should be negative
      val k = freq * dt

      val thisInput = input.outputValue
      val lastOutput = outputValue

      val thisOutput = (lastOutput - freq*(thisInput - lastInput))/(1 + k)

      lastInput = thisInput

      thisOutput
    }

    val children: List[Synth] = input :: cutoff :: Nil
  }



  class LowPass(val input: Synth, val cutoff: Synth) extends Synth {

    override def calculate(): Double = {
      val kPre = sampleRate / (twoPi * cutoff.outputValue.enforcedPositive) // Maybe should be negative
      val k = if (kPre.isInfinity) Double.MaxValue else kPre

      val thisInput = input.outputValue
      val lastOutput = outputValue

      (thisInput + k * lastOutput) / (1 + k)
    }

    val children: List[Synth] = input :: cutoff :: Nil
  }


  class Integrator(val input: Synth) extends ModifyingSynth {
    private var value: Double = 0.0

    override def calculate(): Double = {
      value += input.outputValue * dt
      value
    }
  }

  class Differentiator(val input: Synth) extends ModifyingSynth {
    private var lastValue: Double = 0.0

    override def calculate(): Double = {

      val thisValue = input.outputValue
      val output = (thisValue - lastValue)*sampleRate

      lastValue = thisValue

      output
    }
  }


}
