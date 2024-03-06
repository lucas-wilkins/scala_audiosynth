package examples.generators

import server.quickstart.play
import synth.simple.Square

object squarewave {


  def main(args: Array[String]): Unit = {
    play(0.1 * new Square(440))
  }
}
