package server

import synth.base.AtomicSynth

object control {
  class Stop() extends Exception

  class TimeLimit(val endTime: Double) extends AtomicSynth {
    override def calculate(): Double = {

      if (time > endTime) throw new Stop

      0.0
    }
  }
}
