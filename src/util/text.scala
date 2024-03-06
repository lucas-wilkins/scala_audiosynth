package util

import synth.base.{ModifyingSynth, Synth}
import util.conversions.dbl2conv

object text {

  def nth(int: Int): String = {
    int match {
      case 1 => "1st"
      case 2 => "2nd"
      case 3 => "3rd"
      case _ => s"${int}th"
    }
  }

  /* TODO: Make this usable */
  class TextWaveform(val input: Synth, chars: Int = 20) extends ModifyingSynth {
    def calculate(): Double = {

      val value = input.outputValue
      val nChars = (value.clipMagnitudeOne * chars).toInt

      val line = if (nChars > 0) {
        " "*chars + "|" + "*"*nChars + " "*(chars - nChars)
      } else {
        " "*(chars - nChars) + "*"*nChars + "|" + " "*chars
      }

      println(line)

      value
    }
  }


}
