package sygs.pe.ag

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.ag.submodules._
import sygs.pe.dfe._

/**
  * This module contains the three submodules to handle the communication
  * between the Data Flow Engine and the BRAMS, it connects the three submodules together
  */
class SyGSAddressGeneratorIO(numberOfAccumulators: Int,
                             variablesMemBanks: Int,
                             multiplierQueueSize: Int,
                             accumulatorQueueSize: Int,
                             dividerQueueSize: Int,
                             exp: Int,
                             sign: Int,
                             accumulatorMemAddressWidth: Int,
                             variablesMemAddressWidth: Int) extends Bundle {

  val start = Input(Bool())
  val canProceed = Input(Bool())
  val backwards = Input(Bool())

  //in order: columnIndex, numberOfNonZeros, diagonalValueIndex,
  //          diagonalValueColumnIndex, diagonalValue, rowValue,
  //          initSum, totals, colors/rows
  val accumulatorMemResponses =  Vec(numberOfAccumulators, Vec(9, Flipped(Decoupled(UInt((exp + sign).W)))))
  val accumulatorMemRequests =  Vec(numberOfAccumulators, Vec(9, Decoupled(
    new MemRequestIO(exp + sign, accumulatorMemAddressWidth))))

  val variablesMemResponses = Vec(numberOfAccumulators, Flipped(Decoupled(UInt((exp + sign).W))))

  //in order: variableWrite, variableRead
  val variablesMemRequests = Vec(numberOfAccumulators, Vec(2, Decoupled(
    new MemRequestIO(exp + sign, log2Up(variablesMemBanks) + variablesMemAddressWidth))))

  val dataFlowEngineIO = Flipped(new SyGSDFEIO(numberOfAccumulators, exp, sign))

  val computing = Output(Bool())
  val busy = Output(Bool())
}

class SyGSAddressGenerator(numberOfAccumulators: Int,
                           variablesMemBanks: Int,
                           multiplierQueueSize: Int,
                           accumulatorQueueSize: Int,
                           dividerQueueSize: Int,
                           metadataQueueSize: Int,
                           columnIndicesQueueSize: Int,
                           exp: Int,
                           sign: Int,
                           accumulatorMemAddressWidth: Int,
                           variablesMemAddressWidth: Int
                      ) extends Module {

  val io = IO(new SyGSAddressGeneratorIO(numberOfAccumulators, variablesMemBanks, multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign, accumulatorMemAddressWidth, variablesMemAddressWidth))

  private val requestGeneratorsComputing = Wire(Vec(numberOfAccumulators, Bool()))
  private val requestGeneratorsBusy = Wire(Vec(numberOfAccumulators, Bool()))

  for(i <- 0 until numberOfAccumulators) {

    val metadataMemRequestGenerator = Module(new SyGSMetadataMemReqGen(exp, sign, accumulatorMemAddressWidth))
    val dataMemRequestGenerator = Module(new SyGSDataMemReqGen(exp, sign, accumulatorMemAddressWidth, metadataQueueSize))
    val variableMemRequestGenerator = Module(new SyGSVariableMemReqGen(variablesMemBanks, exp, sign, variablesMemAddressWidth, columnIndicesQueueSize))

    metadataMemRequestGenerator.connect(this, i)
    dataMemRequestGenerator.connect(this, i)
    variableMemRequestGenerator.connect(this, i)

    metadataMemRequestGenerator.connect(dataMemRequestGenerator, variableMemRequestGenerator)

    dataMemRequestGenerator.connect(variableMemRequestGenerator)

    requestGeneratorsComputing(i) := metadataMemRequestGenerator.io.computing || dataMemRequestGenerator.io.busy || variableMemRequestGenerator.io.busy
    requestGeneratorsBusy(i) := metadataMemRequestGenerator.io.busy || dataMemRequestGenerator.io.busy || variableMemRequestGenerator.io.busy
  }

  io.computing := requestGeneratorsComputing.reduce(_ || _)
  io.busy := requestGeneratorsBusy.reduce(_ || _)
}
