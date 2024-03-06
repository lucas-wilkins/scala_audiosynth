package util

import java.awt.geom.Line2D
import java.awt.{Graphics, Graphics2D}

import javax.swing.{JFrame, WindowConstants}
import synth.base.Synth

object scope {


  class Scope(val child: Synth, val sampleLength: Int,
              val threshold: Double = 0, val holdTime: Double = 0.1, val useRisingEdge: Boolean = true,
              val externalTrigger: Option[Synth]) extends Synth {

    private val triggerSynth = externalTrigger.getOrElse(child)

    private var lastTriggerValue: Double = 0
    private var state: Int = 0

    private var currentSampleIndex: Int = 0
    private var currentHoldTime: Double = 0

    private val sample: Array[Double] = Array.fill(sampleLength)(0.0)
    private var endTime: Double = 0

    private val scopeDisplay = new ScopeDisplay(640, 320, sampleLength)

    def beginSampling(): Unit = {
      state = 1
      currentSampleIndex = 0
      currentHoldTime = 0
    }

    def updateDisplay(): Unit = {
      scopeDisplay.updateData(sample)
      scopeDisplay.repaint()
    }

    def calculate(): Double = {

      val triggerValue = triggerSynth.outputValue
      val currentValue = child.outputValue

      // Not recording
      if (state == 0) {

        if (useRisingEdge) {
          // Rising edge
          if ((lastTriggerValue <= threshold) && (triggerValue > threshold)) {
            beginSampling()
          }
        } else {
          // Falling edge
          if ((lastTriggerValue >= threshold) && (triggerValue < threshold)) {
            beginSampling()
          }
        }
      }


      if (state == 1) {


        if (currentSampleIndex  >= sampleLength) {
          state = 2
          endTime = currentHoldTime
          updateDisplay()
        } else {
          sample(currentSampleIndex) = currentValue
          currentSampleIndex += 1
          currentHoldTime += dt
        }
      }

      // Recorded but perhaps still holding
      if (state == 2) {
        currentHoldTime += dt
        if (currentHoldTime >= holdTime) {
          state = 0
        }
      }

      lastTriggerValue = triggerValue

      currentValue
    }


    val children: List[Synth] = child :: Nil


  }

  class ScopeDisplay(width: Int, height: Int, length: Int) extends JFrame("Scope") {

    setSize(width, height)
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    setLocationRelativeTo(null)

    private var data: Array[Double] = Array.fill(length)(0.0)

    def drawLines(g: Graphics): Unit = {
      val g2d = g.asInstanceOf[Graphics2D];

      g2d.drawLine(120, 50, 360, 50);
      g2d.draw(new Line2D.Double(59.2d, 99.8d, 419.1d, 99.8d));
      g2d.draw(new Line2D.Float(21.50f, 132.50f, 459.50f, 132.50f));
    }

    override def paint(g: Graphics): Unit = {
      super.paint(g)
      drawLines(g)
    }

    def updateData(d: Array[Double]): Unit = {
      data = d
    }
  }
}
