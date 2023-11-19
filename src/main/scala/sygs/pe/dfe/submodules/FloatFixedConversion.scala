package sygs.pe.dfe.submodules

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import spatial_templates.Float

class FixedToDouble(width: Int, point: Int, fpExp: Int, fpMant: Int) extends Module {
  val fpType = FixedPoint(width.W, point.BP)
  val in = IO(Input(fpType))
  val out = IO(Output(Float(fpExp,fpMant)))
  val expOffset = ((1 << fpExp-1)-1).U.asTypeOf(UInt(fpExp.W))

  val uSign = in(width-1).asTypeOf(UInt(1.W))
  val fixed = in.asTypeOf(UInt(width.W))
  val noTwosCompl = Mux(uSign === 1.U,  ~(fixed - 1.U), fixed)

  val msb = PriorityEncoder(Reverse(noTwosCompl))
  val b = (width-1).U - msb
  val shiftVal = Mux(b > point.U, b - point.U, point.U - b)
  val absShift = Mux(b > point.U, noTwosCompl, noTwosCompl << shiftVal).asTypeOf(UInt(width.W))

  val mantissa = (absShift(width - 1 - point, 0) << ((fpMant-1)-point).U).asTypeOf(UInt((fpMant-1).W))
  val exponent = Mux(b > point.U, expOffset + shiftVal, expOffset - shiftVal)
  out.bits := Cat(uSign, exponent, mantissa)
}

class DoubleToFixed(width: Int, point: Int, fpExp: Int, fpMant: Int) extends Module {
  val fpType = FixedPoint(width.W, point.BP)
  val in = IO(Input(Float(fpExp,fpMant)))
  val out = IO(Output(fpType))
  val expOffset = ((1 << fpExp-1)-1).U.asTypeOf(UInt(fpExp.W))
  val one = FixedPoint(1, width.W, point.BP)
  val exponent = in.bits((width-1)-1, (fpMant-1)).asTypeOf(UInt(fpExp.W))
  val shiftVal = Mux(exponent > expOffset, exponent - expOffset, expOffset - exponent)
  val mantissa = in.bits((fpMant-1)-1,0)
  val sign = in.bits(width-1)
  val mantissaPlusOne = mantissa.asTypeOf(UInt(width.W)) | (1.U << (fpMant-1).U).asTypeOf(UInt(width.W))
  val mantissaShift = ((fpMant-1)-point)
  val fixedVal = Mux(exponent > expOffset,
    Mux(shiftVal > mantissaShift.U, mantissaPlusOne << (shiftVal - mantissaShift.U), mantissaPlusOne >> (mantissaShift.U - shiftVal)),
    mantissaPlusOne >> (shiftVal + mantissaShift.U))
  out := Mux(sign, (~fixedVal)+1.U, fixedVal).asTypeOf(fpType)
}
