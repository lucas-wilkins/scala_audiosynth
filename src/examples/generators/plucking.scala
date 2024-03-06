package examples.generators

import server.quickstart.play
import synth.pluck.SoftPlucker
import synth.simple.Square

object plucking {
  def main(args: Array[String]): Unit = {

    val clock = new Square(1)
    val plucker = new SoftPlucker(clock, 100, 0.05, 0.001)

    play(plucker)
  }
}
