package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import spatial_templates.Arithmetic.FloatArithmetic._

/**
  * This module implements the reciprocal operation on Floating Point
  */
class FloatInverter(exp: Int, sig: Int) extends Module {

  val io = IO(new Bundle {

    val input = Flipped(Decoupled(Float(exp, sig)))

    val out = Decoupled(Float(exp, sig))
  })

  private val one = Wire(Float(exp, sig))
  one.bits := BigInt("00" + "1" * (exp - 1) + "0" * (sig - 1), 2).U

  private val shouldStart = io.input.valid && (io.out.ready || !io.out.valid)
  private val result = one / (io.input.bits, shouldStart)
  io.out.bits := result.bits
  io.out.valid := result.valid

  io.input.ready := result.ready && (io.out.ready || !io.out.valid)
}
