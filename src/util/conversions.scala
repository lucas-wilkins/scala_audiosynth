package util

object conversions {
  class Conversion(val d: Double) {

    /* Convert BMP to Hz */
    def bpm: Double = d / 60

    def enforcedZeroToOne: Double = {
      if (d < 0) {
        0
      } else if (d > 1) {
        1
      } else {
        d
      }
    }

    def clipMagnitudeOne: Double = {
      if (d < -1) {
        -1
      } else if (d > 1) {
        1
      } else {
        d
      }
    }

    def enforcedPositive: Double = {
      if (d < 0) 0 else d
    }
  }

  implicit def dbl2conv(d: Double): Conversion = new Conversion(d)
  implicit def int2conv(i: Int): Conversion = new Conversion(i)


}
