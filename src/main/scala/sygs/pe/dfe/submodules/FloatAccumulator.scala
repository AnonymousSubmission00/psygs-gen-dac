package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import spatial_templates.Arithmetic.FloatArithmetic._

/**
  * This module implements the accumulation of Floating Points
  * The computation is assumed to complete in one clock cycle
  * It takes as input a starting value to initialize the accumulation
  * Note that the accumulation is always a subtraction
  */
class FloatAccumulator(exp: Int, sign: Int, useBlackBoxAdder64: Boolean) extends Module {

  val io = IO(new Bundle() {


    val in1 = Flipped(Decoupled(Float(exp, sign)))

    val initInputs = Flipped(Decoupled(new Bundle() {

      val init = Float(exp, sign)
      val cycles = UInt((exp + sign).W)
    }))

    val out = Decoupled(Float(exp, sign))
  })

  val midResultsReg1 = Reg(Float(exp, sign))
  val midResultsReg2 = Reg(Float(exp, sign))
  val midResultValid = Reg(Bool())
  val resultReg = Reg(Valid(Float(exp, sign)))
  val cyclesReg = RegInit(UInt((exp + sign).W), 0.U)

  private val shouldInitialize = io.initInputs.valid && cyclesReg === 0.U && (io.out.ready || !io.out.valid) && !midResultValid
  when(shouldInitialize) {

    midResultsReg1 := io.initInputs.bits.init
    midResultsReg2.bits := 0.U
    resultReg.bits := io.initInputs.bits.init
    resultReg.valid := io.initInputs.bits.cycles === 0.U
    cyclesReg := io.initInputs.bits.cycles
  }.elsewhen(cyclesReg > 0.U && io.in1.valid) {

    if(useBlackBoxAdder64) {
      
      val adderBlackBox = Module(new FloatAdderBlackBox64())
      
      val in1 = io.in1.bits
      val in1_sgn = in1.bits(in1.getWidth - 1)
      val in1_neg = Cat(~in1_sgn, in1.bits(in1.getWidth - 2, 0)).asTypeOf(in1)

      adderBlackBox.io.a := midResultsReg1.bits
      adderBlackBox.io.b := in1_neg.bits
      adderBlackBox.io.pushin := io.in1.valid

      midResultsReg2.bits := adderBlackBox.io.r
      midResultsReg1 := midResultsReg2
    }
    else {

      midResultsReg2 := midResultsReg1 - io.in1.bits
      midResultsReg1 := midResultsReg2
    }

    midResultValid := cyclesReg === 1.U
    cyclesReg := cyclesReg - 1.U
  }.elsewhen(cyclesReg === 0.U && !resultReg.valid) {

    if (useBlackBoxAdder64) {

      val adderBlackBox = Module(new FloatAdderBlackBox64())

      adderBlackBox.io.a := midResultsReg1.bits
      adderBlackBox.io.b := midResultsReg2.bits
      adderBlackBox.io.pushin := midResultValid

      resultReg.bits.bits := adderBlackBox.io.r
      resultReg.valid := adderBlackBox.io.pushout
    }
    else {
      resultReg.bits := midResultsReg1 + midResultsReg2
    }

    resultReg.valid := midResultValid
    midResultValid := false.B
  }.otherwise {

    resultReg.bits := resultReg.bits
    resultReg.valid := Mux(io.out.ready, false.B, resultReg.valid)
    cyclesReg := cyclesReg
  }

  io.out.bits := resultReg.bits
  io.out.valid := resultReg.valid

  io.initInputs.ready := cyclesReg === 0.U && (io.out.ready || !io.out.valid) && !midResultValid
  io.in1.ready := cyclesReg > 0.U
}
