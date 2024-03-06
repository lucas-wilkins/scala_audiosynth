package synth

import synth.base.Synth
import util.conversions.dbl2conv

object pluck {
  class Plucker(val clock: Synth, val frequency: Synth, val halfLife: Synth) extends Synth {
    var x: Double = 0.0
    var v: Double = 0.0

    var lastClock = 0.0

    private val ln2 = math.log(2)
    private val twoPi = 2 * math.Pi

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      val freq = twoPi * frequency.outputValue.enforcedPositive
      val damp = ln2 / (halfLife.outputValue.enforcedPositive * freq)

      val deltaT = dt

      x += v * deltaT
      v -= deltaT * freq * (freq * x + damp * v)

      if ((lastClock < 0) && (thisClock >= 0)) {
        x = 1.0
        v = 0.0
      }

      lastClock = thisClock

      x
    }

    val children: List[Synth] = clock :: frequency :: halfLife :: Nil

  }


  class SoftPlucker(val clock: Synth, val frequency: Synth, val halfLife: Synth, val pluckTime: Synth) extends Synth {
    var x: Double = 0.0
    var v: Double = 0.0

    var lastClock = 0.0

    private val ln2 = math.log(2)
    private val twoPi = 2*math.Pi

    private var state = 0

    override def calculate(): Double = {

      if (state == 1) {
        val freq = twoPi * frequency.outputValue.enforcedPositive
        val damp = ln2 / (halfLife.outputValue.enforcedPositive * freq)

        val deltaT = dt

        x += v * deltaT
        v -= deltaT * freq * (freq * x + damp * v)
      } else {
        x += dt/pluckTime.outputValue
        if (x > 1) {
          x = 1
          state = 1
        }
      }

      val thisClock = clock.outputValue
      if ((lastClock < 0) && (thisClock >= 0)) {
        v = 0.0
        state = 0
      }

      lastClock = thisClock
      x

    }

    val children: List[Synth] = clock :: frequency :: halfLife :: Nil
  }
}
