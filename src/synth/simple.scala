package synth

import sequence.misc.Doublable
import synth.base.{AtomicSynth, Constant, Synth}
import util.conversions.dbl2conv

object simple {

  val twoPi: Double = 2 * math.Pi


  trait Oscillator extends Synth {
    val freq: Synth

    def outputValue: Double

    val children: List[Synth] = freq :: Nil
  }

  class Sine(val freq: Synth, val phaseFraction: Double = 0) extends Oscillator {

    var state: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    override def calculate(): Double = {
      state += 2 * dt * freq.outputValue
      while (state > 1) {
        state -= 2
      }

      //println(2 * dt * freq.outputValue)

      math.sin(scala.math.Pi * state)
    }
  }

  class MultiSine(val fundamental: Synth, val amplitudePhasePairs: (Synth, Synth)*) extends Synth {

    var state: Double = -1

    val harmonicsWithFundamental: List[(Synth, Synth)] =
      (new Constant(1), new Constant(0)) :: amplitudePhasePairs.toList

    override def calculate(): Double = {
      state += 2 * dt * fundamental.outputValue
      while (state > 1) {
        state -= 2
      }


      harmonicsWithFundamental
        .zipWithIndex
        .map(x => x._1._1.outputValue*scala.math.sin((x._2+1)*scala.math.Pi*(state + 2*x._1._2.outputValue)))
        .foldLeft(0.0)(_+_)

    }

    val children: List[Synth] = fundamental :: amplitudePhasePairs.toList.flatMap(x => List(x._1, x._2))
  }

  class NormalisedMultiSine(val fundamental: Synth, val amplitudePhasePairs: (Synth, Synth)*) extends Synth {

    var state: Double = -1

    val harmonicsWithFundamental: List[(Synth, Synth)] =
      (new Constant(1), new Constant(0)) :: amplitudePhasePairs.toList

    override def calculate(): Double = {
      state += 2 * dt * fundamental.outputValue
      while (state > 1) {
        state -= 2
      }

      val totalAmplitude =
        harmonicsWithFundamental
          .map(_._1.outputValue)
          .foldLeft(0.0)(_+_)

      harmonicsWithFundamental
        .zipWithIndex
        .map(x => x._1._1.outputValue*scala.math.sin((x._2+1)*scala.math.Pi*(state + 2*x._1._2.outputValue)))
        .foldLeft(0.0)(_+_)
        ./(totalAmplitude)

    }

    val children: List[Synth] = fundamental :: amplitudePhasePairs.toList.flatMap(x => List(x._1, x._2))
  }

  class Saw(val freq: Synth, val phaseFraction: Double = 0) extends Oscillator {
    var value: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    override def calculate(): Double = {
      value += 2 * dt * freq.outputValue
      while (value > 1) {
        value -= 2
      }

      value
    }
  }

  class Triangle(val freq: Synth, val phaseFraction: Double = 0) extends Oscillator {
    var value: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    override def calculate(): Double = {
      value += 2 * dt * freq.outputValue
      while (value > 1) {
        value -= 2
      }

      2 * math.abs(value) - 1
    }
  }

  class Square(val freq: Synth, val phaseFraction: Double = 0) extends Oscillator with Doublable {
    var value: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    override def calculate(): Double = {
      value += 2 * dt * freq.outputValue
      while (value > 1) {
        value -= 2
      }

      if (value < 0) 1 else -1 // Start high, go low
    }
  }

  abstract class TriggeringSquare(val freq: Synth, val phaseFraction: Double = 0) extends Oscillator with Doublable {
    var value: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    var lastOutput: Int = 0
    override def calculate(): Double = {

      value += 2 * dt * freq.outputValue
      while (value > 1) {
        value -= 2
      }

      val output = if (value < 0) 1 else -1 // Start high, go low

      // Rising edge
      if ((lastOutput <= 0) && (output > 0)) {

        onTrigger()
      }

      lastOutput = output

      output
    }

    def onTrigger(): Unit

  }

  class PWM(val freq: Synth, val duty: Synth, val phaseFraction: Double = -1) extends Oscillator {
    var value: Double = (2 * phaseFraction.enforcedZeroToOne) - 1

    override def calculate(): Double = {
      value += 2 * dt * freq.outputValue
      while (value > 1) {
        value = -1
      }

      if (value < 2 * duty.outputValue - 1) 1 else -1 // Start high, go low
    }

    override val children: List[Synth] = freq :: duty :: Nil
  }


}
