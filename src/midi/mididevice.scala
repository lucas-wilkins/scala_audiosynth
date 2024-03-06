package midi

import javax.sound.midi.{MidiDevice, MidiMessage, MidiSystem, Receiver}
import midi.widgets.{ClockedToggle, SliderKnob, MidiControllable, Toggle}
import synth.base.Synth
import util.text.nth

import scala.collection.mutable

object mididevice {

  object MidiController {
    def printMidiDevices(): Unit = {

      for (device <- MidiSystem.getMidiDeviceInfo) {
        println(s"${device.getName}, version=${device.getVersion}")
        println(s"  Vendor: ${device.getVendor}")
        println(s" Details: ${device.getDescription}")
      }
    }
  }

  case class MIDIDeviceException(args: String) extends Exception(args)

  def deviceByName(name: String, index: Int = 0): MidiDevice = {
    val matches =
      MidiSystem
        .getMidiDeviceInfo
        .filter(_.getName.toLowerCase()==name.toLowerCase())

    try {
      MidiSystem.getMidiDevice(matches(index))
    }catch {
      case _: Exception => throw MIDIDeviceException(s"Cannot find ${nth(index+1)} device with name '${name}'")
    }
  }

  class DecodedMessage(val message: MidiMessage) {
    private val intMessage = message.getMessage.toList.map(_.toInt).lift
    val header: Option[Int] = intMessage(0)
    val buttonID: Option[Int] = intMessage(1)
    val value: Option[Int] = intMessage(2)

    override def toString: String = s"${header}, ${buttonID}, ${value}"
  }

  class MidiController(val name: String, val index: Int = 0, val showUnboundMessage: Boolean=true) extends Thread with Receiver {

    private var keepRunning: Boolean = true

    private val device = deviceByName(name, index)

    private val connectedSynths: mutable.HashMap[Int, MidiControllable] = new mutable.HashMap[Int, MidiControllable]

    // This handles midi message, in true Oracle fashion it is called send
    def send(msg: MidiMessage, timeStamp: Long): Unit = {
      val decoded = new DecodedMessage(msg)

      decoded.buttonID match {
        case Some(id) => {

          val target = connectedSynths.get(id)

          target match {
            case Some(controllable) => {

              decoded.value match {
                case Some(value) => {
                  controllable.setValue(value)
                }
                case None => println("MIDI message to short to interpret")
              }
            }
            case None => if (showUnboundMessage) println(s"MIDI input ${id} is currently unbound")
          }
        }
        case None =>
      }

    }

    def close(): Unit = {}

    if (device.isOpen) {
      throw MIDIDeviceException(s"${name} is already open.")
    } else {
      device.open()

      val transmitter = device.getTransmitter

      transmitter.setReceiver(this)
    }

    override def run(): Unit = {
      while (keepRunning) {
        Thread.sleep(1)
      }

      device.close()
    }

    start()

    def knob(id: Int, initialValue: Double = 0.0, showEvents: Boolean = false): SliderKnob = {
      val k = new SliderKnob(s"Knob ${id}", initialValue, showEvents=showEvents)
      connectedSynths(id) = k
      k
    }

    def slider(id: Int, initialValue: Double = 0.0, showEvents: Boolean = false): SliderKnob = {
      val k = new SliderKnob(s"Slider ${id}", initialValue, showEvents=showEvents)
      connectedSynths(id) = k
      k
    }
    def toggle(id: Int, initialValue: Double = 0.0, showEvents: Boolean = false): Toggle = {
      val t = new Toggle(s"Toggle ${id}", initialValue, showEvents=showEvents)
      connectedSynths(id) = t
      t
    }

    def clockedToggle(id: Int, clock: Synth, period: Int,
                      initialBeat: Int = 0,
                      onLow: Boolean = false,
                      initialValue: Double = 0.0,
                      showEvents: Boolean = false): ClockedToggle = {

      val t = new ClockedToggle(s"ClockedToggle ${id}", clock, period, initialBeat, onLow, initialValue, showEvents=showEvents)
      connectedSynths(id) = t
      t
    }
  }

  def main(args: Array[String]): Unit = {
    MidiController.printMidiDevices()

    val thread = new MidiController("nanoKONTROL2", 1)

  }
}
