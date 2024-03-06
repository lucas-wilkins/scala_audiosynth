package filter

import jdk.jfr.Frequency
import synth.base.Synth
import util.conversions.dbl2conv

object advanced {

  trait FixedLengthFIFOBase {
    def apply(index: Int): Double
    def push(value: Double): Unit
  }

  class NullFIFO extends FixedLengthFIFOBase {
    def apply(index: Int): Double = 0.0
    def push(value: Double): Unit = {}
  }

  class FixedLengthFIFO(val length: Int) extends FixedLengthFIFOBase {
    //println(s"Creating FIFO with length ${length}")
    val data: Array[Double] = Array.fill(length)(0.0)
    var currentZeroPosition: Int = 0

    def apply(index: Int): Double = {
      val arrayIndex = (length + currentZeroPosition - index) % length
      //println(s"Fetching ${arrayIndex}")
      data(arrayIndex)
    }

    def push(value: Double): Unit = {
      currentZeroPosition += 1
      currentZeroPosition %= length
      //println(s"Setting ${currentZeroPosition}")
      data(currentZeroPosition) = value
    }

  }

  trait GeneralisedLinearFilter extends Synth {

    val input: Synth

    def nSamples: Int
    def inputHistory: FixedLengthFIFOBase
    def outputHistory: FixedLengthFIFOBase
    def inputWeightings: List[(Int, Double)]
    def outputWeightings: List[(Int, Double)]

    private def inputComponent(kv: (Int, Double)): Double = inputHistory(kv._1) * kv._2
    private def outputComponent(kv: (Int, Double)): Double = outputHistory(kv._1) * kv._2

    override def calculate(): Double = {
      inputHistory.push(input.outputValue)

      val output =
        inputWeightings.map(inputComponent).foldLeft(0.0)(_ + _) -
          outputWeightings.map(outputComponent).foldLeft(0.0)(_ + _)

      outputHistory.push(output)

      output
    }

  }

  class DelayLine(val input: Synth, val delayTime: Double) extends GeneralisedLinearFilter {

    var nSamples: Int = (delayTime * sampleRate).toInt

    var inputHistory: FixedLengthFIFOBase = new FixedLengthFIFO(nSamples)
    val outputHistory: FixedLengthFIFOBase = new NullFIFO()

    val inputWeightings: List[(Int, Double)] = Map(-1 -> 1.0).toList
    val outputWeightings: List[(Int, Double)] = Nil

    override def onSampleRateChange(): Unit = {
      nSamples = (delayTime * sampleRate).toInt
      //println(s"Reassigning sample time to n=${nSamples}")
      inputHistory = new FixedLengthFIFO(nSamples)
    }

    val children: List[Synth] = input :: Nil

  }


  val twoPi: Double = 2*math.Pi
  trait SecondOrderFilter extends Synth {
    val input: Synth
    val frequency: Synth
    val damping: Synth

    var yMinus1: Double = 0.0
    var yMinus2: Double = 0.0

    var xMinus1: Double = 0.0
    var xMinus2: Double = 0.0

    // Filter has the form gainCorrection * (p + qs + rs^2 / 1 + as + bs^2)

    var omega: Double = 1.0
    var zeta: Double = 1.0

    def gainCorrection: Double
    def a: Double
    def b: Double
    def p: Double
    def q: Double
    def r: Double

    override def calculate(): Double = {

      // Set the local variables
      omega = twoPi * frequency.outputValue.enforcedPositive
      zeta = 0.707 * damping.outputValue.enforcedZeroToOne // Don't use good value because exactly sqrt2 is a singularity

      val rate = sampleRate

      val x = input.outputValue

      val dx = rate * (x - xMinus1)
      val ddx = rate * rate * (x - 2*xMinus1 + xMinus2)

      // f is the x based term, p + qs + rs^2
      val f = p*x + q*dx + r*ddx

      // y terms are messier, because one of them is what we're working out
      val aScaled = a * rate
      val bScaled = b * rate * rate

      // g is the 1 + as + bs^2 terms, without the latest y value (which we are calculating now)
      val g = aScaled * yMinus1 - 2*bScaled*yMinus1 + bScaled*yMinus2

      // Scale for unit output
      val factor = gainCorrection / (1 + aScaled + bScaled)

      val output = factor * (f - g)


      // Update history used to calculate values
      yMinus2 = yMinus1
      yMinus1 = output

      xMinus2 = xMinus1
      xMinus1 = x

      //println(output)

      // Return
      output
    }
    val children: List[Synth] = input :: frequency :: damping :: Nil

  }

  class SecondOrderLowPass(val input: Synth, val frequency: Synth, val damping: Synth) extends SecondOrderFilter {

    override def gainCorrection: Double = 2*zeta*math.sqrt(1 - zeta*zeta)

    override def a: Double = 2*zeta/omega

    override def b: Double = 1/(omega*omega)

    override val p: Double = 1

    override val q: Double = 0

    override val r: Double = 0
  }

  class SecondOrderHighPass(val input: Synth, val frequency: Synth, val damping: Synth) extends SecondOrderFilter {

    /*NOT
    WORKING
    (PROBABLY)*/

    override def gainCorrection: Double = 2*zeta*math.sqrt(1 - zeta*zeta)

    override def a: Double = 2*zeta/omega

    override def b: Double = 1/(omega*omega)

    override def p: Double = 0

    override def q: Double = 0

    override def r: Double = 1/(omega*omega)
  }

  class SecondOrderBandPass(val input: Synth, val frequency: Synth, val damping: Synth) extends SecondOrderFilter {

    /*NOT
    WORKING
    (PROBABLY)*/

    override def gainCorrection: Double = 1

    override def a: Double = 2*zeta/omega

    override def b: Double = 1/(omega*omega)

    override def p: Double = 0

    override def q: Double = 2 * zeta / omega

    override def r: Double = 0
  }
}
