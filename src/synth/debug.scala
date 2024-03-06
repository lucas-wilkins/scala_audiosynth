package synth

import synth.base.{ModifyingSynth, Synth, UnaryOperator}

object debug {
  class PrintTriggerTime(input: Synth) extends Synth {
    private var lastValue = 0.0

    override def calculate(): Double = {
      val value = input.outputValue

      if ((value > 0) && (lastValue <= 0)) {
        println(s" Rising: ${time}")
      }

      if ((value < 0) && (lastValue >= 0)) {
        println(s"Falling: ${time}")
      }

      lastValue = value

      value
    }

    override def outputValue: Double = {
      input.outputValue
    }

    val children: List[Synth] = input :: Nil

  }

  class Poll(val input: Synth, val pollDelta: Double = 1.0, text: String = "") extends Synth {

    private val pollText = if (text == "") None else Some(text)

    if (pollDelta <= 0.0) {
      throw new RuntimeException(s"Cannot poll at rate ${pollDelta}")
    }

    val children: List[Synth] = input :: Nil

    private var nextPollTime: Double = 0.0

    override def calculate(): Double = {
      while (nextPollTime < time) {
        nextPollTime += pollDelta
        println(s"Poll ${pollText.getOrElse(input)} = ${input.outputValue}")
      }

      input.outputValue
    }
  }
}
