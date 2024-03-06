package server

import java.nio.ByteBuffer

import javax.sound.sampled.{AudioFormat, AudioSystem, DataLine, LineUnavailableException, SourceDataLine}
import server.control.Stop
import synth.base.Synth
import util.conversions.dbl2conv

import scala.reflect.io.File



class SamplingThread(
      val sampleRate: Int = 44100,
      val bufferTime: Double = 0.1,
      val outputFilename: Option[String] = None)
    extends Thread {

  private val shortFactor: Double = Short.MaxValue.toDouble
  private val shortLength: Int = 2

  val bufferLength: Int = (bufferTime * sampleRate).toInt
  val bufferLengthBytes: Int = bufferLength * shortLength
  val dt: Double = 1.0 / sampleRate


  private var synths: List[Synth] = Nil
  private var keepGoing: Boolean = true

  def add(synth: Synth): Unit = {
    synths = synth :: synths

    val uniqueSynths = synths.map(_.allChildren).foldLeft(Set[Synth]())(_ ++ _)
    uniqueSynths.foreach(_.sampleRate = sampleRate)

  }

  def remove(synth: Synth): Unit = {
    synths = synths.filterNot(_ == synth)
  }

  override def run(): Unit = {
    try {
      val format = new AudioFormat(sampleRate, 16, 1, true, true)
      val info = new DataLine.Info(classOf[SourceDataLine], format, bufferLengthBytes)

      // Audio output
      if (!AudioSystem.isLineSupported(info)) throw new LineUnavailableException()

      val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
      line.open(format)

      line.start()

      // File output
      //val file = outputFilename.map(new File(_))

      println("Playing...")

      while (keepGoing) {
        try {

          val cBuf = ByteBuffer.allocate(bufferLengthBytes)
          val uniqueSynths = synths.map(_.components).foldLeft(Set[Synth]())(_ ++ _)

          /*
          println("   ")
          println("   ")
          println("   ")
          uniqueSynths.foreach(println)
          // */

          for (i <- 0 until bufferLength) {
            uniqueSynths.foreach(_.doCalculation())
            uniqueSynths.foreach(_.update())
            val doubleValue = synths.map(_.outputValue.clipMagnitudeOne).foldLeft(0.0)(_ + _)
            cBuf.putShort((doubleValue * shortFactor).toShort)
          }

          line.write(cBuf.array, 0, cBuf.position)

          try {
            // Wait for buffer to empty
            while (line.getBufferSize() - line.available() > bufferLengthBytes) {
              Thread.sleep(1)
            }
          } catch {
            case e: InterruptedException =>
          }
        } catch {
          case e: Stop => keepGoing = false
        }
      }

      line.drain()




    } catch {
      case e: LineUnavailableException => {

        println("Line of that type is not available")
        e.printStackTrace()
        System.exit(-1)

      }
    }
  }
}
