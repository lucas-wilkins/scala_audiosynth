package util

import util.math.ExtendedPolynomial.{formatNumber, formatPower}

object math {
  case class ExtendedPolynomial(coefficients: List[Double], powerOffset: Int) {
    def *(other: ExtendedPolynomial): ExtendedPolynomial = ???

    def +(other: ExtendedPolynomial): ExtendedPolynomial = {
      val (m1, m2) = matchOffsets(other)
      val (p1, p2) = if (m1.coefficients.length > m2.coefficients.length) {
        (m1, ExtendedPolynomial(m2.coefficients.padTo(m1.coefficients.length, 0.0), m2.powerOffset))
      } else {
        (ExtendedPolynomial(m1.coefficients.padTo(m2.coefficients.length, 0.0), m1.powerOffset), m2)
      }
      ExtendedPolynomial((p1.coefficients zip p2.coefficients).map(x => x._1+x._2), m1.powerOffset).reduceRepresentation
    }

    def -(other: ExtendedPolynomial): ExtendedPolynomial = this + (-other)

    def unary_-(): ExtendedPolynomial = ExtendedPolynomial(coefficients.map(-_), powerOffset)

    def shiftRepresentation(n: Int): ExtendedPolynomial =
      ExtendedPolynomial(List.fill(n)(0.0) ::: coefficients, powerOffset - n)

    def reduceRepresentation(): ExtendedPolynomial = {
      coefficients.headOption match {
        case Some(x) => if (x == 0) {
            ExtendedPolynomial(coefficients.tail, powerOffset-1).reduceRepresentation()
          } else {
          this}
        case None => this
      }
    }

    def matchOffsets(other: ExtendedPolynomial): (ExtendedPolynomial, ExtendedPolynomial) = {
      val diff = scala.math.abs(this.powerOffset - other.powerOffset)
      if (this.powerOffset < other.powerOffset) {
        (this.shiftRepresentation(diff), other)
      } else {
        (this, other.shiftRepresentation(diff))
      }
    }

    def powerValueMap: List[(Int, Double)] =
      coefficients
        .zipWithIndex
        .map(x => (x._2 - powerOffset, x._1))
        .filter(_._2 != 0.0)

    def printString(variableName: String): String = {
      val parts = powerValueMap.map(x => (formatNumber(x._2, x._1) + formatPower(variableName, x._1), x._2 > 0))

      if (parts.isEmpty) {
        "0"
      } else {
        val firstPair = parts.head
        val firstString = (if (firstPair._2) "" else "-") + firstPair._1
        val restStrings = parts.tail.map(x =>
          (if (x._2) " + " else " - ") + x._1
        )

        (firstString :: restStrings).mkString("")
      }
    }

    override def toString = printString("x")

    def details: String = s"[coffs: ${coefficients.toString}, power: ${powerOffset}]"
  }

  object ExtendedPolynomial {

    def formatPower(variableName: String, power: Int): String = {
      if (power == 0) {
        ""
      } else if (power == 1) {
        variableName
      } else {
        s"${variableName}^${power}"
      }
    }

    def formatNumber(number: Double, power: Int): String = {
      if (power == 1) {
        if (number == 1) "" else s"${scala.math.abs(number)}"
      } else {
        s"${scala.math.abs(number)}"
      }
    }
  }

  case class RationalFunction(numerator: ExtendedPolynomial, denominator: ExtendedPolynomial) {

  }

  implicit def num2poly(x: Double): ExtendedPolynomial = ExtendedPolynomial(x :: Nil, 0)

  implicit def poly2rat(extendedPolynomial: ExtendedPolynomial): RationalFunction = RationalFunction(extendedPolynomial, 1)

  val linearTerm = ExtendedPolynomial(List[Double](0, 1), 0)

  def main(args: Array[String]): Unit = {

    val a = linearTerm + 1
    val b = linearTerm - 1

    val c = a + b
    println(s"(${a}) + (${b}) = ${a + b}")

    val d = c - c
    println(s"$c - $c = ${d}")



    println(a.details)
    println(b.details)
    println(c.details)
    println(d.details)
  }
}
