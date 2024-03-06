package midi

import synth.base.{AtomicSynth, Synth}

object widgets {
  trait MidiControllable {

    def setValue(value: Int): Unit = {
      if (showEvents) {
        println(s"${channelName}: ${value}")
      }

      doSetValue(value)
    }

    def doSetValue(value: Int): Unit

    def showEvents: Boolean

    def channelName: String

  }

  class SliderKnob(val channelName: String, initialValue: Double = 0.0, val showEvents: Boolean=false) extends AtomicSynth with MidiControllable {
    private var output: Double = initialValue

    override def setValue(value: Int): Unit = {
      if (showEvents) {
        println(s"${channelName}: ${value} (${value/127.0})")
      }

      doSetValue(value)
    }

    def doSetValue(value: Int): Unit = {
      output = value / 127.0

    }

    override def calculate(): Double = {
      output
    }


  }

  class Toggle(val channelName: String, initialValue: Double = 0.0, val showEvents: Boolean = false)
      extends AtomicSynth with MidiControllable {
    private var output: Double = initialValue

    def doSetValue(value: Int): Unit = {
      if (value == 127) {
        output = 1 - output
      }
    }

    override def calculate(): Double = {
      output
    }
  }

  class ClockedToggle(
       val channelName: String,
       val clock: Synth,
       val period: Int,
       val startIndex: Int = 0,
       onLow: Boolean = false,
       initialValue: Double = 0.0,
       val showEvents: Boolean)

    extends Synth with MidiControllable {

    private val switchState = if (onLow) 0 else 127
    private var lastClockValue = 0.0

    private var beatIndex: Int = startIndex

    private var targetState: Double = initialValue
    private var actualState: Double = initialValue

    def doSetValue(value: Int): Unit = {
      if (value == switchState) {
        if (targetState > 0.5) {
          targetState = 0
        } else {
          targetState = 1
        }
      }
    }

    override def calculate(): Double = {
      val clockValue = clock.outputValue
      if ((clockValue > 0) && (lastClockValue <= 0)) {

        beatIndex += 1
        if (beatIndex >= period) {
          actualState = targetState
          beatIndex = 0
        }
      }

      lastClockValue = clockValue

      actualState
    }

    override val children: List[Synth] = clock :: Nil
  }
}
