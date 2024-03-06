Scala Audiosynth
================

A suite of sythesiser objects that can be patched together to make complex sounds

* Function Generators: Sine, Saw, Square etc
* Filters of various kinds
* Envelopes, Delay Line etc
* Sequencers
* MIDI Controller Interface
* Simple interface for creating new kinds of syths

Setting up and running
----------------------

1) Clone repository
2) Download IntelliJ community
3) Open repository at base directory in IntelliJ
4) Use IntelliJ's prompts for SDK to install the correct Java and Scala SDKs
5) Files in the `examples` directory can be run

Example
-------

Simple example, a program that plays a 440Hz Square wave with amplitude of 0.1

```scala
object squarewave {

  def main(args: Array[String]): Unit = {
    play(0.1 * new Square(440))
  }
}
```

A more complex example that makes a whole drum and bass loop from scratch.
It uses the sequencer objects, plucking simulations, envelopes, utilities for 
working with musical scales and reverb

```scala

    // Set a clock semiquavers at 180BMP
    val clock = new Square(4 * 180.bpm)

    // Sequences for the drums
    val hatTrigger   = new HoldingSequence(clock, "x-x-x-x-x-x-x-x-")
    val snareTrigger = new HoldingSequence(clock, "----x-------x---")
    val kickTrigger  = new HoldingSequence(clock, "x---------x-----")

    // Drum sounds, damped harmonic oscilator for bass, filtered noise for snare and hats
    val kick = new SoftPlucker(kickTrigger, 45 + 0 * new Sine(1) + new  Sine(10), 0.1, 0.001)

    val snareBase = new SecondOrderBandPass(new WhiteNoise(), frequency = 350 + 20*new Sine(1), damping = 0.5)
    val snare = new LinearADSR(snareTrigger, 0.005, 0.15, 0, 0.1) * snareBase

    val hatBase = new SecondOrderHighPass(new WhiteNoise(), 8000, 0.1)
    val hat = new LinearADSR(hatTrigger, 0.02, 0.1, 0, 0) * hatBase

    // Combine
    val drums = kick + 2 * snare + 2 * hat

    //Define pitches for bass line
    val a :: b :: c :: d :: Nil = List[Interval](unison, minorSecond, majorSecond, minorThird).map(80.0*_.multiplier)

    // the melody, at double clock speed
    val melody = new BasicSequencer(
      clock.double,
      a, a, a, a,
      d, d, d, d,
      d, c, b, b,
      b, b, b, b)

    // Specify which notes are held, and when there is a start
    val onValues = new HoldingSequence(clock.double, "- x x x x-- x x-")

    // bass line with clipped sinewaves
    val bass = new ADSR(onValues, 0.1, 0.1, 1, 0.1) * new Clip(new Sine(melody + new Sine(10)), 1+ new Sine(1))

    // Create the a socket so we can loop back the signal from delay line to make reverb
    val socket = new Socket()
    val reverb = new DelayLine(socket + drums + 0.5*bass, 0.1)

    val feedback = 0.1 * reverb
    socket connect feedback

    // Play the tune
    play(reverb)
```
