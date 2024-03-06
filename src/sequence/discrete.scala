package sequence

import synth.base.Synth

object discrete {

  class BinarySequence(val clock: Synth, val data: String) extends Synth {

    private def charToNum(char: Char): Double = char match {
      case ' ' => 0.0
      case '-' => 0.0
      case _ => 1.0
    }

    val digits: Array[Double] = data.toArray.map(charToNum)

    private var index: Int = digits.length - 1
    private var lastClock: Double = 0.0

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      if ((thisClock > 0) && (lastClock <= 0)) {
        index += 1
        index %= digits.length
      }

      val output = digits(index)
      //println(index, output)

      lastClock = thisClock

      output
    }

    val children: List[Synth] = clock :: Nil
  }


  class HoldingSequence(val clock: Synth, val data: String) extends Synth {
    private def charToNum(char: Char): Int = char match {
      case ' ' => 0
      case '-' => 1
      case 'x' => 2
    }

    val digits: Array[Int] = data.toArray.map(charToNum)

    var outputState: Int = 0

    private var index: Int = digits.length - 1
    private var lastClock: Double = 0.0

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      if ((thisClock > 0) && (lastClock <= 0)) {
        index += 1
        index %= digits.length
        outputState = 0
      }

      lastClock = thisClock


      val outputType = digits(index)

      if (outputType == 0) {
        outputState = 1
        -1
      } else if (outputType == 1) {
        outputState = 1
        1
      } else {
        if (outputState == 0) {
          outputState = 1
          -1
        } else {
          1
        }
      }
    }

    val children: List[Synth] = clock :: Nil
  }

  class BasicDrumSequencer(val clock: Synth, val inputs: (Synth, String)*) extends Synth {

    private def createSynths(synthStringPair: (Synth, String)): Synth = {
      val (synth, string) = synthStringPair
      val seq = new BinarySequence(clock, string)
      synth * seq
    }

    val sequences: List[Synth] = inputs.toList.map(createSynths)

    override def calculate(): Double = {
      sequences
        .map(_.outputValue)
        .foldLeft(0.0)(_+_)
    }

    val children: List[Synth] = sequences

  }
}
