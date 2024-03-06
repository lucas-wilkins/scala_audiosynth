package util

//import scala.collection.script.Index
import scala.util.matching.Regex

object scales {

  trait Scale {
    val a3freq: Double

    def freqByIndex(index: Int): Double = {

      a3freq * scala.math.pow(2.0, index/12.0)
    }
  }

  case class WellTempered(a3freq: Double) extends Scale {

    val lookup = Map(
      'A' -> 0,
      'B' -> 2,
      'C' -> (3 - 12),
      'D' -> (5 - 12),
      'E' -> (7 - 12),
      'F' -> (8 - 12),
      'G' -> (10 - 12)
    )

    val notePatten: Regex = "([abcdefgABCDEFG])([#b]?)([0-9])".r

    def apply(noteName: String): Double = {

      notePatten.findFirstMatchIn(noteName) match {
        case Some(m) => {

          val note = lookup(m.group(1)(0).toUpper)
          val incidental = m.group(2) match {
            case "" => 0
            case "#" => 1
            case "b" => -1
          }

          val octave = m.group(3).toInt

          val index = 12 * (octave - 2) + note + incidental

          freqByIndex(index)


        }
        case None => throw new NoSuchElementException(s"Note ${noteName} is not a valid note of the WellTempered scale.")
      }

    }
  }

  // Index relative to A3 register
  case class Note(index: Int) {
    def frequency(scale: Scale): Double = {
      scale.freqByIndex(index)
    }

    def apply(octave: Int): Note = Note(index + 12*(octave-3))

    def sharp: Note = Note(index + 1)
    def flat: Note = Note(index - 1)

    def octaveLower: Note = Note(index - 12)
    def majorSeventhLower: Note = Note(index - 11)
    def minorSeventhLower: Note = Note(index - 10)
    def majorSixthLower: Note = Note(index - 9)
    def minorSixthLower: Note = Note(index - 8)
    def fifthLower: Note = Note(index - 7)
    def tritoneLower: Note = Note(index - 6)
    def fourthLower: Note = Note(index - 5)
    def majorThirdLower: Note = Note(index - 4)
    def minorThirdLower: Note = Note(index - 3)
    def majorSecondLower: Note = Note(index - 2)
    def minorSecondLower: Note = Note(index - 1)

    def minorSecondHigher: Note = Note(index + 1)
    def majorSecondHigher: Note = Note(index + 2)
    def minorThirdHigher: Note = Note(index + 3)
    def majorThirdHigher: Note = Note(index + 4)
    def fourthHigher: Note = Note(index + 5)
    def tritoneHigher: Note = Note(index + 6)
    def fifthHigher: Note = Note(index + 7)
    def minorSixthHigher: Note = Note(index + 8)
    def majorSixthHigher: Note = Note(index + 9)
    def minorSeventhHigher: Note = Note(index + 10)
    def majorSeventhHigher: Note = Note(index + 11)
    def octaveHigher: Note = Note(index + 12)

  }

  object notes {
    val A: Note = Note(0).octaveHigher
    val As: Note = Note(1).octaveHigher

    val Bb: Note = Note(1).octaveHigher
    val B: Note = Note(2).octaveHigher
    val C: Note = Note(3)
    val Cs: Note = Note(4)

    val Db: Note = Note(4)
    val D: Note = Note(5)
    val Ds: Note = Note(6)

    val Eb: Note = Note(6)
    val E: Note = Note(7)
    val F: Note = Note(8)
    val Fs: Note = Note(9)

    val Gb: Note = Note(9)
    val G: Note = Note(10)
    val Gs: Note = Note(11)

    val Ab: Note = Note(11)
  }

  trait Interval {
    val multiplier: Double
  }

  case class HarmonicInterval(ratioLeft: Int, ratioRight: Int) extends Interval {
    val multiplier: Double = ratioLeft.toDouble / ratioRight
  }


  val unison: Interval = HarmonicInterval(1, 1)
  val octave: Interval = HarmonicInterval(2, 1)
  val perfectFifth: Interval = HarmonicInterval(3, 2)
  val perfectFourth: Interval = HarmonicInterval(4, 3)

  val majorThird: Interval = HarmonicInterval(5, 4)
  val minorThird: Interval = HarmonicInterval(6, 5)

  val majorSeventh: Interval = HarmonicInterval(15, 8)
  val minorSeventh: Interval = HarmonicInterval(16, 9)

  val majorSecond: Interval = HarmonicInterval(9, 8)
  val minorSecond: Interval = HarmonicInterval(16, 15)

  val majorSixth: Interval = HarmonicInterval(8, 5)
  val minorSixth: Interval = HarmonicInterval(5, 3)

  object tritone extends Interval {
    override val multiplier: Double = scala.math.sqrt(2)
  }



}
