package sygs
package pe.dfe

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.dfe.submodules._

/**
  * This module handles the data flow through the computational units.
  * It contains one or more MultiplyAccumulator or one or more Inverters at choice
  * It executes the actual computation of the Gauss-Seidel algorithm's formula
  */
class SyGSDFEIO(numberOfAccumulators: Int,
                multiplierQueueSize: Int,
                accumulatorQueueSize: Int,
                dividerQueueSize: Int,
                exp: Int,
                sign: Int) extends Bundle {

  val rowValues = Vec(numberOfAccumulators, Flipped(Decoupled(Float(exp, sign))))
  val variables = Vec(numberOfAccumulators, Flipped(Decoupled(Float(exp, sign))))

  val initSums = Vec(numberOfAccumulators, Flipped(Decoupled(Float(exp, sign))))
  val numbersOfNonZeros = Vec(numberOfAccumulators, Flipped(Decoupled(UInt((exp + sign).W))))

  val diagonalValues = Vec(numberOfAccumulators, Flipped(Decoupled(Float(exp, sign))))

  val out = Decoupled(Float(exp, sign))
  val producerIndex = Output(UInt(log2Up(numberOfAccumulators).W))
}

class SyGSDFE(val numberOfAccumulators: Int,
              numberOfInverters: Int,
              multiplierQueueSize: Int,
              accumulatorQueueSize: Int,
              dividerQueueSize: Int,
              exp: Int,
              sign: Int,
              useBlackBoxAdder64: Boolean) extends Module {

  assert(numberOfAccumulators > 0)
  assert(numberOfInverters > 0)

  if(numberOfAccumulators > 1) assert(numberOfInverters == 1)
  else if(numberOfInverters > 1) assert(numberOfAccumulators == 1)

  assert(multiplierQueueSize > 0)
  assert(accumulatorQueueSize > 0)
  assert(dividerQueueSize > 0)

  val io = IO(new SyGSDFEIO(numberOfAccumulators, multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign))

  //instantiate Modules
  private val multiplyAccumulators = Seq.fill(numberOfAccumulators)(Module(new FloatMultiplyAccumulator(multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign, useBlackBoxAdder64)))

  multiplyAccumulators.zipWithIndex.foreach { case (m, i) =>

    m.io.rowValue <> io.rowValues(i)
    m.io.variable <> io.variables(i)
    m.io.initSum <> io.initSums(i)
    m.io.numbersOfNonZero <> io.numbersOfNonZeros(i)
    m.io.diagonalValue <> io.diagonalValues(i)
  }

  if(numberOfAccumulators > 1) {

    val inverter = Module(new FloatInverterMIMO(numberOfAccumulators, dividerQueueSize, exp, sign))

    multiplyAccumulators.zipWithIndex.foreach { case (m, i) =>

      inverter.io.diagonalValue(i) <> m.io.diagonalValueOut
      m.io.invertedValue <> inverter.io.out(i)
    }

    //out arbiter
    val resultArbiter = Module(new Arbiter(Float(exp, sign), numberOfAccumulators))

    //setting arbiter
    resultArbiter.io.in.zipWithIndex.foreach { case (in, i) =>

      in.bits := multiplyAccumulators(i).io.out.bits
      in.valid := multiplyAccumulators(i).io.out.valid
      multiplyAccumulators(i).io.out.ready := in.ready
    }

    io.out <> resultArbiter.io.out
    io.producerIndex := resultArbiter.io.chosen
  }
  else {

    val inverters = Seq.fill(numberOfInverters)(Module(new FloatInverter(exp, sign)))
    val inverterInputs = Wire(Vec(numberOfInverters, Decoupled(Float(exp, sign))))
    for((inv, in) <- inverters.zip(inverterInputs)) inv.io.input <> in

    val selected = PriorityMux(inverters.zipWithIndex.map { case (inv, i) => (inv.io.input.ready, i.U) })
    multiplyAccumulators.head.io.diagonalValueOut.ready := false.B

    for(i <- inverterInputs.indices) {

      when(i.U === selected) { inverterInputs(i) <> multiplyAccumulators.head.io.diagonalValueOut }
        .otherwise {

          inverterInputs(i).bits := DontCare
          inverterInputs(i).valid := false.B
        }
    }

    val invertedArbiter = Module(new Arbiter(Float(exp, sign), numberOfInverters))

    invertedArbiter.io.in.zipWithIndex.foreach { case (in, i) => in <> inverters(i).io.out }

    multiplyAccumulators.head.io.invertedValue <> invertedArbiter.io.out

    io.out <> multiplyAccumulators.head.io.out
    io.producerIndex := 0.U
  }
}
