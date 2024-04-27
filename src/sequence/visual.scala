package sequence

import synth.base.Synth
import synth.pitch.{Portmanteau, SimplePortmanteau}

import java.awt.event.{ActionListener, ItemEvent, ItemListener, MouseEvent, MouseListener}
import java.awt.geom.RoundRectangle2D
import java.awt.{BorderLayout, Color, Component, Dimension, Graphics, Graphics2D, GridLayout, Insets, RenderingHints}
import java.util.Optional
import javax.swing.border.AbstractBorder
import javax.swing.{Box, BoxLayout, DefaultListModel, JButton, JCheckBox, JComboBox, JFrame, JLabel, JList, JPanel, JScrollPane, JSlider, JToggleButton, ListSelectionModel, SpringLayout, SwingConstants, UIManager, WindowConstants}

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

  class MutableSynthTrack(val clock: Synth) extends Synth {

    var data: List[Int] = (1 to fullSequencePeriod).toList.map(_ => 0)

    var queued: Option[List[Int]] = None
    def queue(newData: List[Int]): Unit = {
      this.queued = Some(newData)
    }

    /* Main squencer bit */

    private var index: Int = fullSequencePeriod - 1
    private var lastClock: Double = 0.0

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      if ((thisClock > 0) && (lastClock <= 0)) {
        index += 1

        if (index == fullSequencePeriod) {

          queued match {
            case Some(queuedData) => data = queuedData
            case None =>
          }
        }

        index %= fullSequencePeriod

      }

      val output = data(index)

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

  class BlockControls(val mainGUIref: VisualSequencerInterface) extends BasePanel {

    val stage = new UIButton("Stage") {
      override def onClick(): Unit = {}
    }

    val choose = new UIButton("Choose") {
      override def onClick(): Unit = {}

    }

    val fill = new UIButton("Fill") {
      override def onClick(): Unit = {}

    }


    val save = new UIButton("Save") {
      override def onClick(): Unit = {}

    }

    val load = new UIButton("Load") {
      override def onClick(): Unit = {}

    }


    def updateStateVisuals(state: Int): Unit = {
      choose.state = state
    }

    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    this.add(stage)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(choose)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(fill)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(save)
    this.add(Box.createRigidArea(new Dimension(5, 15)))
    this.add(load)

  }

  class Outputs(parent: Synth) {
    val trigger = new MutableSynthTrack(parent)
    val hold = new MutableSynthTrack(parent)
  }

  class VisualSequencer(val clock: Synth) extends Synth {
    val interface = new VisualSequencerInterface(this)


    val channel1 = new Outputs(this)
    val channel2 = new Outputs(this)
    val channel3 = new Outputs(this)
    val channel4 = new Outputs(this)
    val channel5 = new Outputs(this)
    val channel6 = new Outputs(this)
    val channel7 = new Outputs(this)
    val channel8 = new Outputs(this)

    val channel1volRaw = new MutableVolumeSynth(this)
    val channel2volRaw = new MutableVolumeSynth(this)
    val channel3volRaw = new MutableVolumeSynth(this)
    val channel4volRaw = new MutableVolumeSynth(this)
    val channel5volRaw = new MutableVolumeSynth(this)
    val channel6volRaw = new MutableVolumeSynth(this)
    val channel7volRaw = new MutableVolumeSynth(this)
    val channel8volRaw = new MutableVolumeSynth(this)

    val channel1vol = new SimplePortmanteau(channel1volRaw, volumeSlew)
    val channel2vol = new SimplePortmanteau(channel2volRaw, volumeSlew)
    val channel3vol = new SimplePortmanteau(channel3volRaw, volumeSlew)
    val channel4vol = new SimplePortmanteau(channel4volRaw, volumeSlew)
    val channel5vol = new SimplePortmanteau(channel5volRaw, volumeSlew)
    val channel6vol = new SimplePortmanteau(channel6volRaw, volumeSlew)
    val channel7vol = new SimplePortmanteau(channel7volRaw, volumeSlew)
    val channel8vol = new SimplePortmanteau(channel8volRaw, volumeSlew)

    override def children: List[Synth] = clock :: Nil

    // For tracking the beat
    private var beat: Int = 15
    private var lastClock: Double = 0.0

    private var outputState: Double = 1.0

    private val beatFactor: Int = fullSequencePeriod / 64

    override def calculate(): Double = {
      val thisClock = clock.outputValue
      if ((thisClock > 0) && (lastClock <= 0)) {
        beat += 1
        beat %= fullSequencePeriod

        // Update the interface
        interface.setBeat(beat)

        // Output at semiquaver speed
        if (beat % beatFactor == 0) {
          outputState = -outputState
        }
      }

      lastClock = thisClock

      // Return clock in sixteenth notes
      outputState
    }
  }

  object VisualSequencer {
    def clockRateForCPS(cps: Double): Double = {
      fullSequencePeriod*(cps/8)
    }
  }

  class BasePanel extends JPanel {
    setBackground(mainBackgroundColor)
  }


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

  val INACTIVE = 3
  val ACTIVE = 2
  val WAITING = 1
  val CLEAR = 0
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
    var _checked: Boolean = false

    def checked: Boolean = _checked

    def checked_=(checked: Boolean): Unit = {
      _checked = checked
      if (checked) {
        setBackground(activeColor)
      } else {
        setBackground(mainBackgroundColor)
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

  val BEAT_OFF = 0
  val BEAT_HOLD = 2
  val BEAT_ON = 1
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


    def beatType_=(beatType: Int): Unit = {
      _beatType = beatType
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

  class ButtonRow(val beatsPerRow: Int) extends BasePanel {

    val beatFactor: Int = fullSequencePeriod / beatsPerRow
    val halfRowBeats: Int = beatsPerRow / 2

    val buttonsLeft = (1 to halfRowBeats).toList.reverse.map(x => new BeatButton(x.toString, beatsPerRow)).reverse
    val buttonsRight = (halfRowBeats + 1 to beatsPerRow).toList.reverse.map(x => new BeatButton(x.toString, beatsPerRow)).reverse

    val buttons = (buttonsLeft ::: buttonsRight).toArray

    val muteButton = new MuteButton("M") {
      override def onToggle(selected: Boolean): Unit = {
        val muteState = selected
        buttons.foreach(_.setMute(muteState))
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

    /** Get the sequence for this row */
    def state: Array[Int] = {
      (0 until fullSequencePeriod).map(beat =>
        buttons(beat / beatFactor).outputValue
      ).toArray
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

  class SequenceBlock extends BasePanel {

    val tripRows = (1 to 3).toList.map(_ => new ButtonRow(24))
    val quadRows = (1 to 6).toList.map(_ => new ButtonRow(32))
    val pentRows = (1 to 3).toList.map(_ => new ButtonRow(40))


    val rows: List[ButtonRow] = tripRows ::: quadRows ::: pentRows

    this.setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))

    rows.foreach(this.add)

    def setBeat(beat: Int): Unit = {
      rows.foreach(_.updateColors(beat))
    }

    def state: List[Array[Int]] = rows.map(_.state)
  }



  class SequenceSection(var state: Int, val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val sequenceBlock = new SequenceBlock
    val controls = new BlockControls(mainGUIref)


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
      controls.updateStateVisuals(state)
    }

    def setState(newState: Int): Unit = {
      state = newState
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

    val boxes = names.map(new UICheck(_))

    boxes.foreach(_.setBackground(mainBackgroundColor))
    boxes.foreach(add)

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

  }
  class VolumesAndOutputs(val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val volumeControls = (1 to 8).toList.map(_ => new OutputChannelControl())

    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))

    volumeControls.foreach(control => {
      add(control)
      add(Box.createRigidArea(new Dimension(10, 0)))
    }
    )
  }


  class SaveDatum(val name: String, val data: List[Array[Int]]) {
    override def toString: String = name
    private def serialiseRow(rowData: Array[Int]): String = rowData.map(_.toString).mkString
    private def serialise(): String = (name :: data.map(serialiseRow)).mkString("|")

  }


  object SaveDatum {

    def deserialiseRow(s: String): Array[Int] = s.map(_.toInt).toArray
    def deserialise(data: String): Option[SaveDatum] = {
      data.split("|").toList match {
        case name :: sequenceStrings => Some(new SaveDatum(name, sequenceStrings.map(deserialiseRow)))
        case _ => None
      }

    }
  }

  object SequenceFileIO {
    def load(filename: String): List[SaveDatum] = ???

  }
  class SavedSequences(val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val saveListModel = new DefaultListModel[SaveDatum]

    val saveList = new JList(saveListModel)


    saveList.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
    saveList.setLayoutOrientation(JList.VERTICAL);
    saveList.setVisibleRowCount(-1)


    val listScroller = new JScrollPane(saveList);
    listScroller.setPreferredSize(new Dimension(500, 80));

    def addItem(name: String, data: List[Array[Int]]): Unit = {
      saveListModel.addElement(new SaveDatum(name, data))
    }

    (1 to 20).foreach( _ => {
      addItem("Test 1", Nil)
      addItem("Test 2", Nil)
      addItem("Test 3", Nil)
    })


    add(listScroller)
  }
  class LowerControlPanel(val mainGUIref: VisualSequencerInterface) extends BasePanel {
    val volumesAndOutputs = new VolumesAndOutputs(mainGUIref)
    val savedSequences = new SavedSequences(mainGUIref)

    setLayout(new BoxLayout(this, BoxLayout.X_AXIS))
    add(Box.createRigidArea(new Dimension(15, 0)))
    add(volumesAndOutputs)
    add(savedSequences)
    add(Box.createRigidArea(new Dimension(15, 0)))
  }

  class VisualSequencerInterface(val parent: VisualSequencer, saveFile: Option[String]=None) {

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

//    UIManager.setLookAndFeel("com.sun.java.swing.plaf.motif.MotifLookAndFeel")

    // Sound -> Display options
    def setBeat(beat: Int): Unit = {

      sequencer1.setBeat(beat)
      sequencer2.setBeat(beat)
    }


  }
}
