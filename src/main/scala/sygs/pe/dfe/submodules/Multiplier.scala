package sygs.pe.dfe.submodules

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import spatial_templates.Arithmetic.FloatArithmetic._
import spatial_templates._

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

class FixedMultiplier(exp: Int, sign: Int) extends Module {

  // Assuming these vals for now
  val fixedPoint = (exp + sign) / 2
  val fixedWidth = exp + sign

  val io = IO(new Bundle() {

    val inputs = Flipped(Decoupled(new Bundle() {

      val in1 = Float(exp, sign)
      val in2 = Float(exp, sign)
    }))

    val out = Decoupled(Float(exp, sign))
  })
  val toFixed1 = Module(new DoubleToFixed(fixedWidth, fixedPoint, exp, sign))
  val toFixed2 = Module(new DoubleToFixed(fixedWidth, fixedPoint, exp, sign))
  val toFloat = Module(new FixedToDouble(fixedWidth, fixedPoint, exp, sign))
  toFixed1.in := io.inputs.bits.in1
  toFixed2.in := io.inputs.bits.in2
  toFloat.in := toFixed1.out * toFixed2.out
  io.out.bits := toFloat.out
  io.out.valid := io.inputs.valid && io.out.ready

  io.inputs.ready := io.out.ready
}

class FixedMultiplierFixedIO(exp: Int, sign: Int) extends Module {

  // Assuming these vals for now
  val fixedPoint = (exp + sign) / 2
  val fixedWidth = exp + sign

  val io = IO(new Bundle() {

    val inputs = Flipped(Decoupled(new Bundle() {

      val in1 = FixedPoint(fixedWidth.W,fixedPoint.BP)
      val in2 = FixedPoint(fixedWidth.W,fixedPoint.BP)
    }))

    val out = Decoupled(FixedPoint(fixedWidth.W,fixedPoint.BP))
  })

  io.out.bits := io.inputs.bits.in1 * io.inputs.bits.in2
  io.out.valid := io.inputs.valid && io.out.ready

  io.inputs.ready := io.out.ready
}