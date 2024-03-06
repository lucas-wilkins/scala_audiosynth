Scala Audiosynth
================

* Function Generators: Sine, Saw, Square etc
* Filters of various kinds, Delay Line etc
* Sequencers
* MIDI Controller Interface
* Simple interface for creating new syths

Setting up and running
----------------------

1) Clone repository
2) Download IntelliJ community
3) Open repository at base directory in IntelliJ
4) Use IntelliJ's prompts for SDK to install the correct Java and Scala SDKs
5) Files in the `examples` directory can be run

Example
-------

Example of playing a C3 major seventh chord made from sine waves:

```scala
import server.quickstart.play
import synth.base.dbl2synth
import synth.simple.Sine
import util.scales.{WellTempered, minorSeventh, minorThird, perfectFifth}

object chord {


  def main(args: Array[String]): Unit = {

    val scale = WellTempered(440)
    val root: Double = scale("C3")

    val first = new Sine(root)
    val third = new Sine(root raised minorThird)
    val fifth = new Sine(root raised perfectFifth)
    val seventh = new Sine(root raised minorSeventh)

    play(0.1 * (first + third + fifth + seventh))

  }
}
```
