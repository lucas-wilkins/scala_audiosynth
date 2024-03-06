package synth

import util.scales.Interval

object base {

  trait Synth {

    /* Basic algebra */

    def *(other: Synth): Synth = new Times(this, other)
    def +(other: Synth): Synth = new Add(this, other)
    def -(other: Synth): Synth = new Subtract(this, other)
    def /(other: Synth): Synth = new Divide(this, other)
    def ^(other: Synth): Synth = new Power(this, other)
    def unary_-(): Synth = new Negative(this)

    /* Component enumeration */

    def children: List[Synth]

    final def allChildren: List[Synth] = this :: this.children.flatMap(_.allChildren)

    final def components: Set[Synth] = allChildren.toSet

    /* Printing stuff */

    def name: String = this.getClass.getSimpleName
    val useBrackets: Boolean = true
    def stringRender(openBracket: String, closeBracket: String, indent: String, newline: String, currentIndent: String = ""): String = {
      val thisLine = if (useBrackets) {
        currentIndent :: name :: openBracket :: Nil
      } else {
        currentIndent :: name :: Nil
      }
      val nextLines =
        children
          .map(newline + _.stringRender(openBracket, closeBracket, indent, newline, currentIndent + indent))
          .mkString(",")

      if (useBrackets) {
        (thisLine ::: (nextLines :: closeBracket :: Nil)).mkString("")
      } else {
        (thisLine ::: (nextLines :: Nil)).mkString("")
      }
    }

    override def toString = stringRender("[", "]", "", "")

    def prettyPrint(initialIndent: String): String =
      stringRender("[", "]", "  ", "\n", currentIndent = initialIndent)

    /* Pitch shifting */
    def raised(interval: Interval): Synth = this * interval.multiplier
    def lowered(interval: Interval): Synth = this * (1/interval.multiplier)

    /* General transforms */
    def transformed(f: Double => Double): Synth = new Transformed(this, f)

    /* Code for dealing with the simulation speed */
    private var _sampleRate: Int = 44100
    private var _dt: Double = 1.0/_sampleRate
    var time: Double = 0.0

    def sampleRate_=(rate: Int): Unit = {
      _sampleRate = rate
      _dt = 1.0/rate
      onSampleRateChange()
    }
    def sampleRate: Int = _sampleRate
    def dt: Double = _dt
    def onSampleRateChange(): Unit = {}

    /* Main simulation parts */

    private var output: Double = 0.0
    private var nextOutput: Double = 0.0

    def outputValue: Double = output
    def calculate(): Double

    def doCalculation(): Unit = {
      time += dt
      nextOutput = calculate()
    }

    def update(): Unit = {
      output = nextOutput
    }
  }


  trait StatelessSynth extends Synth {
    def calculate(): Double = 0.0
  }


  trait AtomicSynth extends Synth {
    val children: List[Synth] = Nil
  }

  trait ModifyingSynth extends Synth {
    val input: Synth

    val children: List[Synth] = input :: Nil
  }

  trait NullaryOperator extends StatelessSynth {
    val children: List[Synth] = Nil
  }

  trait UnaryOperator extends StatelessSynth {
    val input: Synth
    val children: List[Synth] = input :: Nil
  }

  trait BinaryOperator extends StatelessSynth {
    val s1: Synth
    val s2: Synth
    val children: List[Synth] = s1 :: s2 :: Nil
  }




  class Constant(val const: Double) extends NullaryOperator {
    override def outputValue: Double = const
    override def name: String = s"${const}"

    override val useBrackets: Boolean = false
  }

  implicit def dbl2synth(value: Double): Synth = new Constant(value)
  implicit def int2synth(value: Int): Synth = new Constant(value)


  class Add(val s1: Synth, val s2: Synth) extends BinaryOperator {
    override def outputValue: Double = (s1.outputValue) + (s2.outputValue)
  }

  class Times(val s1: Synth, val s2: Synth) extends BinaryOperator {
    override def outputValue: Double = s1.outputValue * s2.outputValue
  }

  class Subtract(val s1: Synth, val s2: Synth) extends BinaryOperator {
    override def outputValue: Double = (s1.outputValue) - (s2.outputValue)
  }

  class Divide(val s1: Synth, val s2: Synth) extends BinaryOperator {
    override def outputValue: Double = (s1.outputValue) / (s2.outputValue)
  }

  class Power(val s1: Synth, val s2: Synth) extends BinaryOperator {
    override def outputValue: Double = scala.math.pow(s1.outputValue, s2.outputValue)
  }

  class Negative(val input: Synth) extends UnaryOperator {
    override def outputValue: Double = -input.outputValue
  }

  class Transformed(val input: Synth, expression: Double => Double) extends UnaryOperator {
    override def outputValue(): Double = {
      expression(input.outputValue)
    }
  }

  abstract class ExposeVariable[T <: Synth](child: T) extends Synth {
    val children: List[Synth] = child :: Nil

    override def calculate(): Double = {
      exposedValue()
    }

    def exposedValue(): Double

  }

}
