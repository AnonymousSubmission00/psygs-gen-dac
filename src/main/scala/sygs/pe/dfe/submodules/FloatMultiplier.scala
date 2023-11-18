package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import spatial_templates.Arithmetic.FloatArithmetic._

/**
  * This module implements the product of Floating Point
  */
class FloatMultiplier(exp: Int, sign: Int) extends Module {

  val io = IO(new Bundle() {

    val inputs = Flipped(Decoupled(new Bundle() {

      val in1 = Float(exp, sign)
      val in2 = Float(exp, sign)
    }))

    val out = Decoupled(Float(exp, sign))
  })

  io.out.bits := io.inputs.bits.in1 * io.inputs.bits.in2
  io.out.valid := io.inputs.valid && io.out.ready

  io.inputs.ready := io.out.ready
}
