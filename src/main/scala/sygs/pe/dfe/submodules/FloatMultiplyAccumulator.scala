package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import spatial_templates.Arithmetic.FloatArithmetic._

/**
  * This module coordinates the FloatMultiplier and the FloatAccumulator
  */
class FloatMultiplyAccumulator(val multiplierQueueSize: Int, accumulatorQueueSize: Int, dividerQueueSize: Int, exp: Int, sign: Int, useBlackBoxAdder64: Boolean) extends Module {

  assert(multiplierQueueSize > 0)
  assert(accumulatorQueueSize > 0)

  val io = IO(new Bundle() {

    val rowValue = Flipped(Decoupled(Float(exp, sign)))
    val variable = Flipped(Decoupled(Float(exp, sign)))

    val initSum = Flipped(Decoupled(Float(exp, sign)))
    val numbersOfNonZero = Flipped(Decoupled(UInt((exp + sign).W)))

    val diagonalValue = Flipped(Decoupled(Float(exp, sign)))
    val diagonalValueOut = Decoupled(Float(exp, sign))

    val invertedValue = Flipped(Decoupled(Float(exp, sign)))

    val out = Decoupled(Float(exp, sign))
  })

  //Processing Units
  private val multiplier = Module(new FloatMultiplier(exp, sign))
  private val accumulator = Module(new FloatAccumulator(exp, sign, useBlackBoxAdder64))

  //Wires to bundle inputs
  private val multiplierInputs = Wire(Decoupled(new Bundle() {

    val rowValue = Float(exp, sign)
    val variable = Float(exp, sign)
  }))
  private val accumulatorInputs = Wire(Decoupled(new Bundle() {

    val initSum = Float(exp, sign)
    val cycles = UInt((exp + sign).W)
  }))

  //redirecting diagonal value
  private val diagonalValueQueue = Queue(io.diagonalValue, dividerQueueSize)
  diagonalValueQueue.ready := io.diagonalValueOut.ready
  io.diagonalValueOut.bits := diagonalValueQueue.bits
  io.diagonalValueOut.valid := diagonalValueQueue.valid

  //multiplier inputs Queues
  private val rowValuesQueue = Queue(io.rowValue, multiplierQueueSize)
  private val variablesQueue = Queue(io.variable, multiplierQueueSize)
  rowValuesQueue.ready := multiplierInputs.ready && variablesQueue.valid
  variablesQueue.ready := multiplierInputs.ready && rowValuesQueue.valid

  //setting of wires
  multiplierInputs.bits.rowValue := rowValuesQueue.bits
  multiplierInputs.bits.variable := variablesQueue.bits
  multiplierInputs.valid := rowValuesQueue.valid && variablesQueue.valid
  multiplierInputs.ready := multiplier.io.inputs.ready

  //multiplier
  multiplier.io.inputs.bits.in1 := multiplierInputs.bits.rowValue
  multiplier.io.inputs.bits.in2 := multiplierInputs.bits.variable
  multiplier.io.inputs.valid := multiplierInputs.valid


  //queue between accumulator and multiplier
  private val multiplierResultsQueue = Queue(multiplier.io.out, accumulatorQueueSize)
  multiplierResultsQueue.ready := accumulator.io.in1.ready


  //accumulator inputs Queues
  private val initSumsQueue = Queue(io.initSum, accumulatorQueueSize)
  private val numberOfNonZerosQueue = Queue(io.numbersOfNonZero, accumulatorQueueSize)
  initSumsQueue.ready := accumulatorInputs.ready && numberOfNonZerosQueue.valid
  numberOfNonZerosQueue.ready := accumulatorInputs.ready && initSumsQueue.valid

  //setting of wires
  accumulatorInputs.bits.initSum := initSumsQueue.bits
  accumulatorInputs.bits.cycles := numberOfNonZerosQueue.bits - 1.U
  accumulatorInputs.valid := initSumsQueue.valid && numberOfNonZerosQueue.valid
  accumulatorInputs.ready := accumulator.io.initInputs.ready

  //Queue after accumulator and after inverter
  private val accumulatorResultQueue = Queue(accumulator.io.out, dividerQueueSize)
  private val invertedValueQueue = Queue(io.invertedValue, dividerQueueSize)
  accumulatorResultQueue.ready := io.out.ready && invertedValueQueue.valid
  invertedValueQueue.ready := io.out.ready && accumulatorResultQueue.valid

  //accumulator
  accumulator.io.in1.bits := multiplierResultsQueue.bits
  accumulator.io.initInputs.bits.init := accumulatorInputs.bits.initSum
  accumulator.io.initInputs.bits.cycles := accumulatorInputs.bits.cycles
  accumulator.io.in1.valid := multiplierResultsQueue.valid
  accumulator.io.initInputs.valid := accumulatorInputs.valid

  //out
  io.out.bits := accumulatorResultQueue.bits * invertedValueQueue.bits
  io.out.valid := accumulatorResultQueue.valid && invertedValueQueue.valid
}
