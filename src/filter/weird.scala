package filter

import scala.math

import filter.simple.{HighPass, LowPass}
import synth.base.{Constant, Synth}
import util.conversions.dbl2conv

object weird {
  class LevelQuantise(val input: Synth, val levels: Synth) extends Synth {
    override def calculate(): Double = {

      val n = math.abs(levels.outputValue).ceil

      (input.outputValue*n).floor / n
    }

    val children: List[Synth] = input :: levels :: Nil

  }

  /**
   * Quantises the `input` value to nearest multiple of `levels`, do this at a rate specified by `timestep`
   *
   *
   *
   * @param input signal to be quantised
   * @param levels size of the quantisation steps
   * @param timeStep how long to wait between value changes
   */

  class Quantise(val input: Synth, val levels: Synth, val timeStep: Synth = 0) extends Synth {


    var thisTime: Double = Double.PositiveInfinity
    var thisValue: Double = 0.0

    override def calculate(): Double = {

      if (thisTime >= timeStep.outputValue) {

        val n = math.abs(levels.outputValue).ceil

        thisValue = (input.outputValue * n).floor / n

        thisTime = 0.0
      }

      thisTime += dt
      thisValue
    }


      val children: List[Synth] = input :: levels :: timeStep :: Nil
  }



  /**
   * Designed for frequencies, Quantises the `input` frequency to nearest tone
   *
   * @param levels size of the quantisation steps
   * @param timeStep how long to wait between value changes
   */

  class LogQuantise(val input: Synth, val number_of_tones: Double, val root: Synth) extends Synth {

    val base = number_of_tones*scala.math.log(2)/12

    override def calculate(): Double = {
      val rootNote = root.outputValue
      val logValue = scala.math.log(input.outputValue/rootNote)/base

      rootNote * scala.math.exp( scala.math.floor(logValue)*base)
    }


    val children: List[Synth] = input :: root :: Nil
  }

  class Clip(input: Synth, scale: Synth) extends Synth {
    override def calculate(): Double = {

      (input.outputValue * (1 + scale.outputValue.enforcedPositive)).clipMagnitudeOne
    }

    val children = input :: scale :: Nil
  }
}
