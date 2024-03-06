package chaos

import synth.base.{Constant, ExposeVariable, Synth}

object lorentz {

  class LorentzLFO(val sigma: Synth, val rho: Synth, val beta: Synth, val speed: Synth) extends Synth {

    // ρ > 24.74


    val children: List[Synth] = sigma :: rho :: beta :: speed :: Nil

    var x: Double = 2.0
    var y: Double = 1.0
    var z: Double = 1.0

    val x_variable = new ExposeVariable(this) {
      override def exposedValue(): Double = { 0.5 + x/60 }
    }

    val y_variable = new ExposeVariable(this) {
      override def exposedValue(): Double = {0.5 + y/60}
    }

    val z_variable = new ExposeVariable(this) {
      override def exposedValue(): Double = {z/60}
    }


    override def calculate(): Double = {
      val equation_dt = speed.outputValue * dt

      val dxdt = sigma.outputValue * (y - x)
      val dydt = x*(rho.outputValue - z) - y
      val dzdt = x*y - beta.outputValue*z

      x += dxdt * equation_dt
      y += dydt * equation_dt
      z += dzdt * equation_dt

      0.5 + x/60
    }
  }

  def typicalLorentz(speed: Synth): LorentzLFO = {
    // Good numbers for chaos
    //  ρ = 28, σ = 10 and β = 8/3

    new LorentzLFO(
      rho=new Constant(28),
      sigma=new Constant(10),
      beta=new Constant(8/3),
      speed=speed)
  }

}
