package sequence

import synth.base.{StatelessSynth, Synth}

import scala.util.Random

object vcotype {

  class BasicSequencer(val trigger: Synth, val synths: Synth*) extends Synth {
    private val n = synths.length
    private var index: Int = 0
    private var lastTriggerValue: Double = -1

    def calculate(): Double = {
      val triggerValue = trigger.outputValue

      // Rising edge
      if ((lastTriggerValue < 0) && (triggerValue > 0)) {
        index += 1
        index %= n
      }

      lastTriggerValue = triggerValue

      synths(index).outputValue
    }

    val children: List[Synth] = trigger :: synths.toList
  }

  case class FrequencyShift(val child: Synth, val multiple: Double) extends StatelessSynth {
    override def outputValue(): Double = multiple * child.outputValue

    val children: List[Synth] = child :: Nil
  }




  class MutatingSequencer(
         val trigger: Synth,
         val initialNotes: List[Synth],
         val intervals: List[Double],
         val maxShift: Double = 2,
         val mutationFrequency: Int = 1)

      extends Synth {

    private val noteArray: Array[Synth] = initialNotes.map(FrequencyShift(_, 1)).toArray

    private val n = noteArray.length
    private var index: Int = 0
    private var lastTriggerValue: Double = -1

    val rand: Random = scala.util.Random

    def calculate(): Double = {
      val triggerValue = trigger.outputValue

      // Rising edge
      if ((lastTriggerValue < 0) && (triggerValue > 0)) {
        index += 1
        if (index >= n) {
          index = 0
          mutate()
        }
      }

      lastTriggerValue = triggerValue

      noteArray(index).outputValue
    }

    var mutateIndex: Int = 0
    def mutate(): Unit = {
      mutateIndex += 1
      if (mutateIndex >= mutationFrequency) {
        val arrayInd = rand.nextInt(n)
        val intervalInd = rand.nextInt(intervals.length)

        noteArray(arrayInd) = noteArray(arrayInd) match {
          case FrequencyShift(synth, mult) => {

            val test = if (rand.nextBoolean()) {
              mult * intervals(intervalInd)
            } else {
              mult / intervals(intervalInd)
            }

            val newMult =
              if ((test < 1 / maxShift) || (test > maxShift)) {
                1
              } else {
                test
              }

            FrequencyShift(synth, newMult)

          }

          case x => x
        }

        mutateIndex = 0
      }

    }
    val children: List[Synth] = trigger :: initialNotes

  }
}
