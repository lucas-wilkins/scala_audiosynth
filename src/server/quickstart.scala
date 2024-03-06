package server

import server.control.TimeLimit
import synth.base.Synth

object quickstart {

  def play(synth: Synth): Unit = {
    val server = new SamplingThread()
    server.add(synth)
    println(s"Loading:\n${synth.prettyPrint("  ")}")
    server.run()
  }

  def play(synth: Synth, endTime: Double): Unit = {
    val server = new SamplingThread()
    server.add(synth)
    server.add(new TimeLimit(endTime))
    println(s"Loading:\n${synth.prettyPrint("  ")}")
    server.run()
  }

}
