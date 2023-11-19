package sygs.pe.dfe.submodules

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import spatial_templates.Arithmetic.FloatArithmetic._
import spatial_templates._

class MultiplyAccumulatorIO(exp: Int, sign: Int) extends Bundle {

    val rowValue = Flipped(Decoupled(Float(exp, sign)))
    val variable = Flipped(Decoupled(Float(exp, sign)))

    val initSum = Flipped(Decoupled(Float(exp, sign)))
    val numbersOfNonZero = Flipped(Decoupled(UInt((exp + sign).W)))

    val diagonalValue = Flipped(Decoupled(Float(exp, sign)))
    val diagonalValueOut = Decoupled(Float(exp, sign))

    val invertedValue = Flipped(Decoupled(Float(exp, sign)))

    val out = Decoupled(Float(exp, sign))
}

class MultiplyAccumulator(val multiplierQueueSize: Int, accumulatorQueueSize: Int, dividerQueueSize: Int, exp: Int, sign: Int, useBlackBoxAdder64: Boolean) extends Module {

  assert(multiplierQueueSize > 0)
  assert(accumulatorQueueSize > 0)

  val io = IO(new MultiplyAccumulatorIO(exp, sign))
}

/**
 * This module coordinates the FloatMultiplier and the FloatAccumulator
 */
class FloatMultiplyAccumulator(override val multiplierQueueSize: Int, accumulatorQueueSize: Int, dividerQueueSize: Int, exp: Int, sign: Int, useBlackBoxAdder64: Boolean)
  extends MultiplyAccumulator(multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign, useBlackBoxAdder64) {

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

/**
 * This module coordinates the FixedMultiplier and the FixedAccumulator
 */
class FixedMultiplyAccumulator(override val multiplierQueueSize: Int, accumulatorQueueSize: Int, dividerQueueSize: Int, exp: Int, sign: Int, useBlackBoxAdder64: Boolean)
  extends MultiplyAccumulator(multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign, useBlackBoxAdder64) {

  //Processing Units
  private val multiplier = Module(new FixedMultiplierFixedIO(exp, sign))
  private val multiplierOut = Module(new FixedMultiplierFixedIO(exp, sign))
  private val accumulator = Module(new FixedAccumulatorFixedIO(exp, sign, useBlackBoxAdder64))

  //Wires to bundle inputs
  private val multiplierInputs = Wire(Decoupled(new Bundle() {

    val rowValue = FixedPoint((exp+sign).W, ((exp+sign)/2).BP)
    val variable = FixedPoint((exp+sign).W, ((exp+sign)/2).BP)
  }))
  private val accumulatorInputs = Wire(Decoupled(new Bundle() {

    val initSum = FixedPoint((exp+sign).W, ((exp+sign)/2).BP)
    val cycles = UInt((exp + sign).W)
  }))

  //redirecting diagonal value
  private val diagonalValueQueue = Queue(io.diagonalValue, dividerQueueSize)
  diagonalValueQueue.ready := io.diagonalValueOut.ready
  io.diagonalValueOut.bits := diagonalValueQueue.bits
  io.diagonalValueOut.valid := diagonalValueQueue.valid

  //multiplier inputs Queues
  private val rowValuesQueue = Queue(io.rowValue, 1)
  private val variablesQueue = Queue(io.variable, 1)

  private val rowFixedQueue = Module(new Queue(FixedPoint((exp+sign).W, ((exp+sign)/2).BP), 1))
  private val varFixedQueue = Module(new Queue(FixedPoint((exp+sign).W, ((exp+sign)/2).BP), 1))

  // Float to Fixed modules
  val rowValuesToFixed = Module(new DoubleToFixed(exp+sign, (exp+sign)/2, exp, sign))
  val variablesToFixed = Module(new DoubleToFixed(exp+sign, (exp+sign)/2, exp, sign))

  rowValuesQueue.ready := rowFixedQueue.io.enq.ready
  rowValuesToFixed.in := rowValuesQueue.bits
  rowFixedQueue.io.enq.valid := rowValuesQueue.valid
  rowFixedQueue.io.enq.bits := rowValuesToFixed.out

  variablesQueue.ready := varFixedQueue.io.enq.ready
  variablesToFixed.in := variablesQueue.bits
  varFixedQueue.io.enq.valid := variablesQueue.valid
  varFixedQueue.io.enq.bits := variablesToFixed.out

  //setting of wires
  multiplierInputs.bits.rowValue := rowFixedQueue.io.deq.bits
  multiplierInputs.bits.variable := varFixedQueue.io.deq.bits
  multiplierInputs.valid := varFixedQueue.io.deq.valid && rowFixedQueue.io.deq.valid
  multiplierInputs.ready := multiplier.io.inputs.ready
  rowFixedQueue.io.deq.ready := multiplierInputs.ready && varFixedQueue.io.deq.valid
  varFixedQueue.io.deq.ready := multiplierInputs.ready && rowFixedQueue.io.deq.valid

  //multiplier
  multiplier.io.inputs.valid := multiplierInputs.valid
  multiplier.io.inputs.bits.in1 := multiplierInputs.bits.rowValue
  multiplier.io.inputs.bits.in2 := multiplierInputs.bits.variable

  //queue between accumulator and multiplier
  private val multiplierResultsQueue = Queue(multiplier.io.out, 1)
  multiplierResultsQueue.ready := accumulator.io.in1.ready

  //accumulator inputs Queues
  private val initSumsQueue = Queue(io.initSum, 1)

  val initToFixed = Module(new DoubleToFixed(exp+sign, (exp+sign)/2, exp, sign))

  val initFixedQueue = Module(new Queue(FixedPoint((exp+sign).W, ((exp+sign)/2).BP), 1))
  private val numberOfNonZerosQueue = Queue(io.numbersOfNonZero, 1)

  initFixedQueue.io.enq.valid := initSumsQueue.valid
  initToFixed.in := initSumsQueue.bits
  initFixedQueue.io.enq.bits :=  initToFixed.out
  initSumsQueue.ready := initFixedQueue.io.enq.ready
  initFixedQueue.io.deq.ready := accumulatorInputs.ready && numberOfNonZerosQueue.valid

  numberOfNonZerosQueue.ready := accumulatorInputs.ready && initFixedQueue.io.deq.valid

  //setting of wires
  accumulatorInputs.bits.initSum := initFixedQueue.io.deq.bits
  accumulatorInputs.bits.cycles := numberOfNonZerosQueue.bits - 1.U
  accumulatorInputs.valid := initFixedQueue.io.deq.valid && numberOfNonZerosQueue.valid
  accumulatorInputs.ready := accumulator.io.initInputs.ready

  //Queue after accumulator and after inverter
  private val accumulatorResultQueue = Queue(accumulator.io.out, dividerQueueSize)
  private val invertedValueQueue = Queue(io.invertedValue, dividerQueueSize)

  val invertedToFixed = Module(new DoubleToFixed(exp+sign, (exp+sign)/2, exp, sign))
  val invertedFixedQueue = Module(new Queue(FixedPoint((exp+sign).W, ((exp+sign)/2).BP), 1))

  invertedValueQueue.ready := invertedFixedQueue.io.enq.ready
  invertedFixedQueue.io.enq.valid := invertedValueQueue.valid
  invertedToFixed.in := invertedValueQueue.bits
  invertedFixedQueue.io.enq.bits := invertedToFixed.out

  accumulatorResultQueue.ready := multiplierOut.io.inputs.ready && invertedFixedQueue.io.deq.valid// io.out.ready && invertedValueQueue.valid

  invertedFixedQueue.io.deq.ready := multiplierOut.io.inputs.ready && accumulatorResultQueue.valid//io.out.ready && accumulatorResultQueue.valid

  //accumulator
  accumulator.io.in1.bits := multiplierResultsQueue.bits
  accumulator.io.initInputs.bits.init := accumulatorInputs.bits.initSum
  accumulator.io.initInputs.bits.cycles := accumulatorInputs.bits.cycles
  accumulator.io.in1.valid := multiplierResultsQueue.valid
  accumulator.io.initInputs.valid := accumulatorInputs.valid

  multiplierOut.io.inputs.bits.in1 := accumulatorResultQueue.bits
  multiplierOut.io.inputs.bits.in2 := invertedFixedQueue.io.deq.bits
  multiplierOut.io.inputs.valid := invertedFixedQueue.io.deq.valid && accumulatorResultQueue.valid

  val resultFixedQueue = Module(new Queue(FixedPoint((exp+sign).W, ((exp+sign)/2).BP), 1))
  resultFixedQueue.io.enq.valid := multiplierOut.io.out.valid
  resultFixedQueue.io.enq.bits := multiplierOut.io.out.bits
  multiplierOut.io.out.ready := resultFixedQueue.io.enq.ready

  // Fixed to Float
  val toFloat = Module(new FixedToDouble((exp+sign), ((exp+sign)/2), exp, sign))
  toFloat.in := resultFixedQueue.io.deq.bits

  //out
  val resultQueue = Module(new Queue(new Float(exp, sign), 2))
  resultQueue.io.enq.valid := resultFixedQueue.io.deq.valid
  resultQueue.io.enq.bits := toFloat.out
  resultFixedQueue.io.deq.ready := resultQueue.io.enq.ready

  io.out.bits := resultQueue.io.deq.bits //accumulatorResultQueue.bits * invertedValueQueue.bits
  io.out.valid := resultQueue.io.deq.valid //accumulatorResultQueue.valid && invertedValueQueue.valid
  resultQueue.io.deq.ready := io.out.ready
}
