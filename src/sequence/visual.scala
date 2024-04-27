package sequence

import sequence.visual.VisualSequencer.clockRateForCPS
import synth.base.Synth
import synth.pitch.{Portmanteau, SimplePortmanteau}
import synth.simple.{Square, TriggeringSquare}

import java.awt.event.{ActionListener, ItemEvent, ItemListener, MouseEvent, MouseListener}
import java.awt.geom.RoundRectangle2D
import java.awt.{BorderLayout, Color, Component, Dimension, Graphics, Graphics2D, GridLayout, Insets, RenderingHints}
import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Optional
import javax.swing.border.AbstractBorder
import javax.swing.{Box, BoxLayout, DefaultListModel, JButton, JCheckBox, JComboBox, JFrame, JLabel, JList, JPanel, JScrollPane, JSlider, JToggleButton, ListSelectionModel, SpringLayout, SwingConstants, UIManager, WindowConstants}
import scala.collection.convert.ImplicitConversions.`enumeration AsScalaIterator`
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object visual {

  /* Comments are for forward thinking people */

  private val fullSequencePeriod = 960

  private val mainBackgroundColor = new Color(128,128,128)

  private val holdOffColor = new Color(250, 200, 160)
  private val holdOnColor = new Color(250,200,20)

  private val offColor = mainBackgroundColor
  private val onColor = new Color(200,60,60)

  private val triggerOnColor = new Color(10,250,10)
  private val triggerOffColor = new Color(130,250,130)

  private val muteOnColor = new Color(128,128,200)
  private val muteOffColor = mainBackgroundColor

  private val muteTriggerOnColor = new Color(50,80,255)
  private val muteTriggerOffColor = new Color(128,160,255)

  private val muteHoldOnColor = new Color(110,50,255)
  private val muteHoldOffColor = new Color(190,128,255)


  private val muteToggleOnColor = new Color(128,128,255)
  private val activeColor = new Color(40,255,40)
  private val waitingColor = new Color(250, 160, 70)
  private val inactiveColor = new Color(250,80,80)

  private val normalVolumeColor = new Color(30,30,30)
  private val quiteLoudColor = waitingColor
  private val tooLoudColor = inactiveColor

  private val volumeSlew = 10


  /** Sawtooth output. Rising edge from -1 to 1 at zero, decreases to -1 at end of hold  */
  class MutableTriggerTrack(val clock: Synth, tempoBPS: Double) extends Synth {

    var data: Array[Int] = (1 to fullSequencePeriod).map(_ => 0).toArray

    var next = data
    var fillData: Option[Array[Int]] = None

    def stage(newData: Array[Int]): Unit = {
//      println("staging:", newData.toList)
      next = newData
    }

    def fill(data: Array[Int]): Unit = {
      fillData = Some(data)
    }

    /* Main squencer bit */

    private var index: Int = fullSequencePeriod - 1
    private var lastClock: Double = 0.0

    var output: Double = -1
    var slewRate: Double = 1

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      if ((thisClock > 0) && (lastClock <= 0)) {
        index += 1

        if (index == fullSequencePeriod) {

          data = fillData match {
            case Some(fill) => {
              fillData = None
              fill
            }

            case None => next
          }
        }

        index %= fullSequencePeriod


        if (data(index) == BEAT_ON) {
          output = 1


          // Calculate slew rate, need to do a forwards check - this can never be perfect, but it will do
          var holdCount = 1
          var finished = false
          breakable {
            for (rest <- index + 1 until data.length) {
              if (data(rest) == BEAT_HOLD) {
                holdCount += 1
              } else {
                finished = true
                break
              }
            }
          }

          // look into queued sequence if currently not ready
          if (!finished) {

            val fieldToCheck = fillData match {
              case Some(fill) => fill
              case None => next
            }

            breakable {
              for (beatType <- fieldToCheck) {
                if (beatType == BEAT_HOLD) {
                  holdCount += 1
                } else {
                  break
                }
              }
            }
          }

          slewRate = tempoBPS / holdCount


        }

      }

      output -= slewRate * dt

      if (output < -1) {
        output = -1
      }

      lastClock = thisClock

      output
    }

    val children: List[Synth] = clock :: Nil

  }

  class MutableVolumeSynth(clock: Synth) extends Synth {

    override def children: List[Synth] = clock :: Nil

    private var state: Double = 1.0

    def set(volume: Double) = state = volume

    override def calculate(): Double = state
  }

  class BlockControls(val mainGUI: VisualSequencerInterface, parent: SequenceSection) extends BasePanel {

    val stage = new UIButton("Stage") {
      override def onClick(): Unit = {
        state = WAITING
        mainGUI.stage(parent)
      }
    }


    val fill = new UIButton("Fill") {
      override def onClick(): Unit = {
        state = WAITING
        mainGUI.fill(parent)
      }

    }


    val save = new UIButton("Save") {
      override def onClick(): Unit = {
        val sequences = parent.sequenceBlock.sequences
        mainGUI.outputPanel.savedSequences.addSequence(SequencerDatum.createNow(sequences))
      }

    }

    val load = new UIButton("Load") {
      override def onClick(): Unit = {
        val sequences = mainGUI.outputPanel.selectedSequence
        parent.sequenceBlock.sequences = sequences.data
        showStageNeeded()

      }

    }

    def showStageNeeded() = {
      stage.state = INACTIVE
    }

    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    this.add(stage)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(fill)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(save)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(load)

  }

  class VisualSequencer(val tempoCPS: Double, filename: Option[String]=None) {
    val tempoBPS = clockRateForCPS(tempoCPS)
    // For tracking the beat
    private var beat: Int = 0

    val clock: Synth = new TriggeringSquare(tempoBPS) {
      def onTrigger(): Unit = {
        beat += 1
        beat %= fullSequencePeriod

        // Update the interface
        interface.setBeat(beat)

      }
    }

    val interface = new VisualSequencerInterface(this, filename)


    val channel1 = new MutableTriggerTrack(clock, tempoBPS)
    val channel2 = new MutableTriggerTrack(clock, tempoBPS)
    val channel3 = new MutableTriggerTrack(clock, tempoBPS)
    val channel4 = new MutableTriggerTrack(clock, tempoBPS)
    val channel5 = new MutableTriggerTrack(clock, tempoBPS)
    val channel6 = new MutableTriggerTrack(clock, tempoBPS)
    val channel7 = new MutableTriggerTrack(clock, tempoBPS)
    val channel8 = new MutableTriggerTrack(clock, tempoBPS)

    val channel1volRaw = new MutableVolumeSynth(clock)
    val channel2volRaw = new MutableVolumeSynth(clock)
    val channel3volRaw = new MutableVolumeSynth(clock)
    val channel4volRaw = new MutableVolumeSynth(clock)
    val channel5volRaw = new MutableVolumeSynth(clock)
    val channel6volRaw = new MutableVolumeSynth(clock)
    val channel7volRaw = new MutableVolumeSynth(clock)
    val channel8volRaw = new MutableVolumeSynth(clock)

    val channel1vol = new SimplePortmanteau(channel1volRaw, volumeSlew)
    val channel2vol = new SimplePortmanteau(channel2volRaw, volumeSlew)
    val channel3vol = new SimplePortmanteau(channel3volRaw, volumeSlew)
    val channel4vol = new SimplePortmanteau(channel4volRaw, volumeSlew)
    val channel5vol = new SimplePortmanteau(channel5volRaw, volumeSlew)
    val channel6vol = new SimplePortmanteau(channel6volRaw, volumeSlew)
    val channel7vol = new SimplePortmanteau(channel7volRaw, volumeSlew)
    val channel8vol = new SimplePortmanteau(channel8volRaw, volumeSlew)

    val channels = List(
      channel1,
      channel2,
      channel3,
      channel4,
      channel5,
      channel6,
      channel7,
      channel8)

    val volumes = List(
      channel1vol,
      channel2vol,
      channel3vol,
      channel4vol,
      channel5vol,
      channel6vol,
      channel7vol,
      channel8vol)


  }

  object VisualSequencer {
    def clockRateForCPS(cps: Double): Double = {
      fullSequencePeriod*(cps/8)
    }
  }

  /** Base class for all panels */
  class BasePanel extends JPanel {
    setBackground(mainBackgroundColor)
  }


  /** For drawing round buttons */
  class RoundedBorder(val color: Color, val gap: Int) extends AbstractBorder {


    override def paintBorder(c: Component, g: Graphics, x: Int, y: Int, width: Int, height: Int) = {
      val g2d = g.create().asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE)
      g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
      g2d.setColor(color)
      g2d.draw(new RoundRectangle2D.Double(x + 1, y + 1, width - 2, height - 2, gap, gap))
    }

    override def getBorderInsets(c: Component): Insets = {
      getBorderInsets(c, new Insets(gap, gap, gap, gap))
    }

    override def getBorderInsets(c: Component, insets: Insets): Insets = {
      insets.left = gap / 2
      insets.top = gap / 2
      insets.right = gap / 2
      insets.bottom = gap / 2

      insets
    }

    override def isBorderOpaque = false

  }

  abstract class UIToggle(text: String) extends BasePanel with MouseListener {
    var isSelected = false

    val label = new JLabel(text)

    label.setOpaque(true)
    label.setVerticalAlignment(SwingConstants.CENTER)
    label.setHorizontalAlignment(SwingConstants.CENTER)

    label.setBorder(new RoundedBorder(Color.black, 10))

    setLayout(new BorderLayout())
    add(label)
    addMouseListener(this)
    label.addMouseListener(this)

    setLabelColor()

    /* Abstract look option */
    def setLabelColor()

    /* Toggle has do nothing implementation */

    def onToggle(selected: Boolean): Unit = {}


    /* Mouse Listener implements */

    override def mouseClicked(e: MouseEvent): Unit = {}
    override def mousePressed(e: MouseEvent): Unit = {
      isSelected = !isSelected
      onToggle(isSelected)
      setLabelColor()
    }

    override def mouseReleased(e: MouseEvent): Unit = {}

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}


  }

  val CLEAR = 0
  val WAITING = 1
  val ACTIVE = 2
  val INACTIVE = 3

  abstract class UIButton(text: String) extends BasePanel with MouseListener {

    val label = new JLabel(text)

    label.setOpaque(true)
    label.setVerticalAlignment(SwingConstants.CENTER)
    label.setHorizontalAlignment(SwingConstants.CENTER)

    label.setBorder(new RoundedBorder(Color.black, 10))

    setLayout(new BorderLayout())
    add(label)
    addMouseListener(this)
    label.addMouseListener(this)
    label.setBackground(mainBackgroundColor)

    private var _state: Int = CLEAR
    def state: Int = _state
    def state_=(newState: Int) = {
      _state = newState
      label.setBackground(stateColor)
    }

    def onClick(): Unit

    /* Mouse Listener implements */

    def stateColor: Color = {
      state match {
        case CLEAR => mainBackgroundColor
        case WAITING => waitingColor
        case ACTIVE => activeColor
        case INACTIVE => inactiveColor
      }
    }

    override def mouseClicked(e: MouseEvent): Unit = {
      label.setBackground(stateColor)
      onClick()
    }
    override def mousePressed(e: MouseEvent): Unit = {
      label.setBackground(new Color(180,180,180))
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      label.setBackground(stateColor)
    }

    override def mouseEntered(e: MouseEvent): Unit = {

    }

    override def mouseExited(e: MouseEvent): Unit = {}

  }

  class UICheck(text: String) extends UIButton(text) {
    private var _checked: Boolean = false

    def checked: Boolean = _checked

    def checked_=(checked: Boolean): Unit = {
      _checked = checked
      updateColor()
    }

    def updateColor(): Unit = {
      if (checked) {
        label.setBackground(activeColor)
      } else {
        label.setBackground(mainBackgroundColor)
      }

    }

    override def onClick(): Unit = {

      checked = !checked
    }
  }

  class UIVolume extends BasePanel {

    val slider = new UISlider
    val mute = new MuteButton("M") {
      override def onToggle(isMute: Boolean) {
        slider.mute = isMute
      }
    }

    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    this.add(slider)
    this.add(Box.createRigidArea(new Dimension(5, 10)))
    this.add(mute)

    def volume: Double = slider.overallVolume
  }

  class UISlider extends BasePanel with MouseListener {

    val gap = 4

    private var _volume = 0.5
    private var _mute = false

    setBackground(mainBackgroundColor)
    setBorder(new RoundedBorder(Color.black, 10))

    val dims = new Dimension(30, 150)
    setPreferredSize(dims)
    setMaximumSize(dims)
    setSize(dims)


    addMouseListener(this)

    repaint()


    override def paint(g: Graphics) = {

      super.paint(g)

      val g2d = g.create().asInstanceOf[Graphics2D]
      g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE)
      g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
      g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)

      if (mute) {
        g2d.setColor(muteOnColor)
      } else {
        g2d.setColor(_volume match {
          case x if x < 0.8 => normalVolumeColor
          case x if x < 0.9 => quiteLoudColor
          case _ => tooLoudColor
        })
      }


      val drawHeight = ((dims.height - 2*gap)*_volume).toInt
      val startHeight = dims.height - gap - drawHeight
      val shape = new RoundRectangle2D.Double(gap, startHeight, dims.width - 2*gap, drawHeight, 10, 10)

      g2d.fill(shape)


    }

    def nonMuteVolume_=(volume: Double): Unit = {
      _volume = volume

      repaint()
    }

    def nonMuteVolume: Double = _volume

    def mute_=(mute: Boolean) = {
      _mute = mute
      repaint()
    }
    def mute = _mute

    def overallVolume: Double = if (mute) 0.0 else nonMuteVolume
    override def mouseClicked(e: MouseEvent): Unit = {}

    override def mousePressed(e: MouseEvent): Unit = {}

    override def mouseReleased(e: MouseEvent): Unit = {
      val y = e.getY

      var fraction: Double = (y-5.0)/(getHeight-10.0)

      fraction = if (fraction < 0) 0.0 else fraction
      fraction = if (fraction > 1) 1.0 else fraction

      nonMuteVolume = 1-fraction
    }

    override def mouseEntered(e: MouseEvent): Unit = {}

    override def mouseExited(e: MouseEvent): Unit = {}

  }

  class MuteButton(text: String) extends UIToggle(text) {
    def setLabelColor(): Unit = {
      if (isSelected) {
        label.setBackground(muteToggleOnColor)
      } else {
        label.setBackground(mainBackgroundColor)
      }
    }
  }

  // These need to be have these numerical values so that we can use max to reduce them
  val BEAT_OFF = 0
  val BEAT_HOLD = 1
  val BEAT_ON = 2
  class BeatButton(text: String, beatsPerRow: Int) extends UIToggle(text) {

    setPreferredSize(new Dimension(40*(fullSequencePeriod/beatsPerRow), 30))

    private var mute = false
    private var active = false
    private var _beatType = BEAT_OFF

    def setLabelColor(): Unit = {

        if (mute) {
          if (active) {
            label.setBackground(
              _beatType match {
              case BEAT_ON => muteTriggerOnColor
              case BEAT_HOLD => muteHoldOnColor
              case BEAT_OFF => muteOnColor
              case _ => Color.red
            })
          } else {

            label.setBackground(
              _beatType match {
                case BEAT_ON => muteTriggerOffColor
                case BEAT_HOLD => muteHoldOffColor
                case BEAT_OFF => muteOffColor
                case _ => Color.red
              })
          }
        } else {
          if (active) {
            label.setBackground(
              _beatType match {
                case BEAT_ON => triggerOnColor
                case BEAT_HOLD => holdOnColor
                case BEAT_OFF => onColor
                case _ => Color.red
              })
          } else {
            label.setBackground(
              _beatType match {
                case BEAT_ON => triggerOffColor
                case BEAT_HOLD => holdOffColor
                case BEAT_OFF => offColor
                case _ => Color.red
              })
          }
        }

    }

    def onChanged(): Unit = {}

    def beatType_=(beatType: Int): Unit = {
      val changed = beatType != _beatType

      _beatType = beatType

      if (changed) {
        onChanged()
      }

      setLabelColor()
    }

    def beatType: Int = _beatType

    def setMute(isMute: Boolean): Unit = {
      mute = isMute
      setLabelColor()
    }

    override def mousePressed(e: MouseEvent): Unit = {
      if (e.getButton == MouseEvent.BUTTON1) {
        beatType = beatType match {
          case BEAT_ON => BEAT_OFF
          case BEAT_HOLD => BEAT_ON
          case BEAT_OFF => BEAT_ON
        }
      }

      if (e.getButton == MouseEvent.BUTTON3) {
        beatType = beatType match {
          case BEAT_ON => BEAT_HOLD
          case BEAT_HOLD => BEAT_OFF
          case BEAT_OFF => BEAT_HOLD
        }
      }

      setLabelColor()
    }

    def setActive(isActive: Boolean): Unit = {
      if ((isActive && !active) || (!isActive && active)) {
        active = isActive
        setLabelColor()
      }
    }

    def outputValue: Int = {
      if (mute) {
        0
      } else {
        _beatType
      }
    }


  }

  class ButtonRow(parent: SequenceBlock, val beatsPerRow: Int) extends BasePanel {

    val beatFactor: Int = fullSequencePeriod / beatsPerRow
    val halfRowBeats: Int = beatsPerRow / 2

    // Subsequences used to create the state output
    val beatOnSequence: List[Int] = 2 :: (1 until beatFactor).toList.map(_ => 1)
    val beatHoldSequence: List[Int] = (0 until beatFactor).toList.map(_ => 1)
    val beatOffSequence: List[Int] = (0 until beatFactor).toList.map(_ => 0)

    val buttonsLeft = (1 to halfRowBeats)
      .toList
      .reverse
      .map(x => new BeatButton(x.toString, beatsPerRow){
        override def onChanged() = {
          parent.onChanged()
        }})
      .reverse


    val buttonsRight = (halfRowBeats + 1 to beatsPerRow)
      .toList
      .reverse
      .map(x => new BeatButton(x.toString, beatsPerRow){
        override def onChanged() = {
          parent.onChanged()
        }})
      .reverse

    val buttons = (buttonsLeft ::: buttonsRight).toArray

    val muteButton = new MuteButton("M") {
      override def onToggle(selected: Boolean): Unit = {
        val muteState = selected
        buttons.foreach(_.setMute(muteState))
        parent.onChanged()
      }
    }

    val holdAll = new UIButton("H") {
      override def onClick(): Unit = {
        // Are all the buttons hold or trigger, if so, we remove hold
        val allHoldOrTrigger = !buttons.map(_.beatType == BEAT_OFF).foldLeft(false)(_ || _)

        if (allHoldOrTrigger) {
          // Switch all holds to off
          buttons.foreach( button => {
            if (button.beatType == BEAT_HOLD) {
              button.beatType = BEAT_OFF
            }
          })
        } else {
          // Switch all offs to hold
          buttons.foreach( button => {
            if (button.beatType == BEAT_OFF) {
              button.beatType = BEAT_HOLD
            }
          })
        }
      }
    }

    val duplicate = new UIButton("D") {
      // Copy first half into second half
      override def onClick(): Unit = {
        buttonsLeft.zip(buttonsRight).foreach {
          case (from, to) => to.beatType = from.beatType
        }
      }
    }

    /* Arrangement of components */
    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

    add(muteButton)
    add(Box.createRigidArea(new Dimension(5, 0)))
    buttonsLeft.foreach(add)
    add(Box.createRigidArea(new Dimension(5, 0)))
    buttonsRight.foreach(add)
    add(Box.createRigidArea(new Dimension(5, 0)))
    add(duplicate)
    add(holdAll)

    /** Get the sequence need for the audio side */
    def state: Array[Int] = {
      buttons
        .map(_.beatType)
        .flatMap {
          case BEAT_ON => beatOnSequence
          case BEAT_HOLD => beatHoldSequence
          case _ => beatOffSequence}
    }

    /** Sequence for the fileIO side - set*/
    def sequence_=(sequenceData: Array[Int]): Unit = {
      buttons.zip(sequenceData.toList).foreach {
        case (button, state) => button.beatType = state
      }
    }

    /** Sequence for the fileIO side - get*/
    def sequence: Array[Int] = {
      buttons.map(_.beatType).toArray
    }

    def updateColors(beat: Int): Unit = {

      buttons.zipWithIndex.foreach(butInd => {
          if (beat / beatFactor == butInd._2) {
            butInd._1.setActive(true)
          } else{
            butInd._1.setActive(false)
          }
        }
      )

    }
  }

  class SequenceBlock(parent: SequenceSection) extends BasePanel {

    val tripRows = (1 to 3).toList.map(_ => new ButtonRow(this, 24))
    val quadRows = (1 to 6).toList.map(_ => new ButtonRow(this, 32))
    val pentRows = (1 to 3).toList.map(_ => new ButtonRow(this, 40))


    val rows: List[ButtonRow] = tripRows ::: quadRows ::: pentRows

    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    rows.foreach(this.add)

    def setBeat(beat: Int): Unit = {
      rows.foreach(_.updateColors(beat))
    }

    /** Data needed for the audio side */
    def state: List[Array[Int]] = rows.map(_.state)

    /** Data needed for fileIO side - set */
    def sequences_=(seqs: List[Array[Int]]): Unit = {
      rows.zip(seqs).foreach {
        case (row, data) => row.sequence = data
      }
    }

    /** Data needed for fileIO side - get */
    def sequences: List[Array[Int]] = rows.map(_.sequence)

    def onChanged() = {
      parent.controls.showStageNeeded()
    }
  }



  class SequenceSection(var selectedState: Int, val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val sequenceBlock = new SequenceBlock(this)
    val controls = new BlockControls(mainGUIref, this)


    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

    add(Box.createRigidArea(new Dimension(15, 0)))
    add(sequenceBlock)
    add(controls)
    add(Box.createRigidArea(new Dimension(15, 0)))

    updateStateVisuals()

    def setBeat(beat: Int): Unit = {
      sequenceBlock.setBeat(beat)
    }

    def updateStateVisuals(): Unit = {

    }

    def setSelectedState(newState: Int): Unit = {
      selectedState = newState
      updateStateVisuals()
    }

  }

  class OutputCheckGrid() extends BasePanel {
    val gridLayout = new GridLayout(6, 2)
    setLayout(gridLayout)

    val seqTypes = List(3,3,3,4,4,4,4,4,4,5,5,5)
    val names = (1 to 12).toList.zip(seqTypes).map {
      case (i, t) => s"${i} (${t})"
    }

    val boxes = names.map(new UICheck(_)).toArray

    boxes.foreach(add)

    def mapping: Array[Boolean] = {
      boxes.map(_.checked)
    }

  }
  class OutputChannelControl extends BasePanel {
    val volumeSlider = new UIVolume()
    val channelList = new OutputCheckGrid()


    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

//    volumeSlider.setOrientation(SwingConstants.VERTICAL)
//    volumeSlider.setBackground(mainBackgroundColor)

    add(volumeSlider)
    this.add(Box.createRigidArea(new Dimension(5, 0)))
    add(channelList)

    def mapping = channelList.mapping

  }

  class VolumesAndOutputs(val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val volumeControls = (1 to 8).toList.map(_ => new OutputChannelControl())

    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

    volumeControls.foreach(control => {
      add(control)
      add(Box.createRigidArea(new Dimension(10, 0)))
    }
    )

    (0 until 6).foreach(index => {
      volumeControls(index).channelList.boxes(index+3).checked = true
    })

    def mapping: List[Array[Boolean]] = {
      volumeControls.map(_.mapping)
    }
  }


  class SequencerDatum(val name: String, val data: List[Array[Int]]) {
    override def toString: String = name
    private def serialiseRow(rowData: Array[Int]): String = rowData.map(_.toString).mkString
    def serialise(): String = (name :: data.map(serialiseRow)).mkString("|")

  }


  object SequencerDatum {

    def deserialiseRow(s: String): Array[Int] = {
      val data = s.toList.map(_.toString).map(_.toInt)
      data.toArray
    }

    def deserialise(data: String): Option[SequencerDatum] = {
      val split = data.split("\\|").toList


      split match {
        case name :: sequenceStrings => Some(new SequencerDatum(name, sequenceStrings.map(deserialiseRow)))
        case _ => {
          println("Read sequence line failed")
          None
        }
      }

    }

    def createNow(data: List[Array[Int]]): SequencerDatum = {
      val date = new java.util.Date()
      val formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      val name = formatter.format(date)

      new SequencerDatum(name, data)
    }

  }

  object SequenceFileIO {

    def load(filename: String): List[SequencerDatum] = {
      try {
        val source = Source.fromFile(filename)

        val data = source
          .getLines
          .flatMap(SequencerDatum.deserialise)
          .toList
        source.close()

        data

      } catch {

        case _: Exception  => {
          println("Read failed")
          Nil
        }
      }
    }

    def save(filename: String, sequences: List[SequencerDatum]) = {
      try {

        new PrintWriter(filename) {

          sequences.foreach(sequence => {
            write(sequence.serialise())
            write("\n")
          })

          close()
        }

      } catch {
        case _: Exception  => println("Write failed")
      }
    }
  }

  class SavedSequences(val mainGUIref: VisualSequencerInterface) extends BasePanel {

    val saveListModel = new DefaultListModel[SequencerDatum]
    val saveList = new JList(saveListModel)

    saveList.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    saveList.setLayoutOrientation(JList.VERTICAL);
    saveList.setVisibleRowCount(-1)


    val listScroller = new JScrollPane(saveList);
    listScroller.setPreferredSize(new Dimension(500, 80));

    add(listScroller)

    load()

    def save(): Unit = {
      val sequences = saveListModel.elements.toList

      SequenceFileIO.save(mainGUIref.saveFile, sequences)
    }

    def addSequence(sequence: SequencerDatum): Unit = {
      saveListModel.addElement(sequence)
      save()
    }

    def load(): Unit = {
      saveListModel.clear()
      SequenceFileIO.load(mainGUIref.saveFile).foreach(saveListModel.addElement)
    }

    def selectedSequence: SequencerDatum = {
      saveList.getSelectedValue
    }
  }


  class LowerControlPanel(val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val volumesAndOutputs = new VolumesAndOutputs(mainGUIref)
    val savedSequences = new SavedSequences(mainGUIref)

    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
    add(Box.createRigidArea(new Dimension(15, 0)))
    add(volumesAndOutputs)
    add(savedSequences)
    add(Box.createRigidArea(new Dimension(15, 0)))

    def selectedSequence: SequencerDatum = {
      savedSequences.selectedSequence
    }

    def mapping = volumesAndOutputs.mapping
  }

  class VisualSequencerInterface(val parent: VisualSequencer, saveFilename: Option[String]=None) {

    val saveFile = saveFilename match {
      case Some(filename) => filename
      case None => "sequence_data.seq"
    }

    val mainFrame = new JFrame("Sequencer")
    mainFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)


    val mainPanel = new BasePanel
    mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS))

    val outputPanel = new LowerControlPanel(this)

    val sequencer1 = new SequenceSection(ACTIVE, this)
    val sequencer2 = new SequenceSection(INACTIVE, this)

    mainFrame.add(mainPanel)

    mainPanel.add(Box.createRigidArea(new Dimension(0, 10)))
    mainPanel.add(sequencer1)
    mainPanel.add(Box.createRigidArea(new Dimension(0, 15)))
    mainPanel.add(sequencer2)
    mainPanel.add(Box.createRigidArea(new Dimension(0, 15)))
    mainPanel.add(outputPanel)
    mainPanel.add(Box.createRigidArea(new Dimension(0, 10)))

    mainFrame.pack()
    mainFrame.show()

    // Sound -> Display options
    def setBeat(beat: Int): Unit = {

      sequencer1.setBeat(beat)
      sequencer2.setBeat(beat)
    }



    def fill(sequencer: SequenceSection): Unit = {
      val seqs = sequences(sequencer)
      parent.channels.zip(seqs).foreach {
        case (channel, sequence) => channel.fill(sequence)
      }
    }

    def stage(sequencer: SequenceSection): Unit = {
      val seqs = sequences(sequencer)
      parent.channels.zip(seqs).foreach {
        case (channel, sequence) => channel.stage(sequence)
      }
    }

    val emptySequence = (0 until fullSequencePeriod).map(_ => 0).toArray
    def sequences(sequencer: SequenceSection): List[Array[Int]] = {
      val sequences = sequencer.sequenceBlock.state
      val mappings = outputPanel.mapping

      // Create list of sequences we need to merge for each output channel
      val includedSequences = mappings
        .map(mapping => {
          mapping
            .zip(sequences)
            .filter(_._1)
            .map(_._2)
        })

      includedSequences.map(sequences => {
        sequences.foldLeft(emptySequence) {
          case (a: Array[Int], b: Array[Int]) => a.zip(b).map {
            case (x: Int, y: Int) => scala.math.max(x, y)
          }}
        })

    }

  }

}
