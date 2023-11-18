package sygs
package pe.ag.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.ag.SyGSAddressGenerator

/**
  * This module sends requests to the variable memory interface to get the vector x's component
  * needed by the Data Flow Engine for the computation.
  * It also handles the updates of the x's components by sending writing requests to the shared BRAM banks
  */
class SyGSVariableMemReqGen(variablesMemBanks: Int,
                             exp: Int,
                             sign: Int,
                             variablesMemAddressWidth: Int,
                             columnIndicesQueueSize: Int) extends Module {

  //variableMemBanks must be a power of 2
  assert(variablesMemBanks == Math.pow(2, log2Up(variablesMemBanks)))

  val io = IO(new Bundle() {

    val rows = Flipped(Decoupled(UInt((exp + sign).W)))

    val columnIndexIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val diagonalValueColumnIndexIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val variableIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val resultVariableIn = Flipped(Decoupled(UInt((exp + sign).W)))

    val variableReadMemRequest = Decoupled(new MemRequestIO(exp + sign, log2Up(variablesMemBanks) + variablesMemAddressWidth))
    val variableWriteMemRequest = Decoupled(new MemRequestIO(exp + sign, log2Up(variablesMemBanks) + variablesMemAddressWidth))

    val variableOut = Decoupled(UInt((exp + sign).W))

    val busy = Output(Bool())
  })

  private val busyReg = RegInit(Bool(), false.B)

  private val rowsQueue = Queue(io.rows, 1)
  private val columnIndicesQueue = Queue(io.columnIndexIn, columnIndicesQueueSize)
  private val diagonalValueColumnIndexQueue = Queue(io.diagonalValueColumnIndexIn, 1)
  private val resultVariableQueue = Queue(io.resultVariableIn, 1)

  private val variableReadAddress = Wire(UInt(variablesMemAddressWidth.W))
  private val bankReadAddress = Wire(UInt(log2Up(variablesMemBanks).W))
  variableReadAddress := columnIndicesQueue.bits >> log2Up(variablesMemBanks)
  bankReadAddress := columnIndicesQueue.bits - (variableReadAddress << log2Up(variablesMemBanks)).asUInt

  io.variableReadMemRequest.bits.write := false.B
  io.variableReadMemRequest.bits.dataIn := 0.U
  io.variableReadMemRequest.bits.addr := (bankReadAddress << variablesMemAddressWidth).asUInt + variableReadAddress
  io.variableReadMemRequest.valid := columnIndicesQueue.valid

  columnIndicesQueue.ready := io.variableReadMemRequest.ready

  private val variableWriteAddress = Wire(UInt(variablesMemAddressWidth.W))
  private val bankWriteAddress = Wire(UInt(log2Up(variablesMemBanks).W))
  variableWriteAddress := diagonalValueColumnIndexQueue.bits >> log2Up(variablesMemBanks)
  bankWriteAddress := diagonalValueColumnIndexQueue.bits - (variableWriteAddress << log2Up(variablesMemBanks)).asUInt

  io.variableWriteMemRequest.bits.write := true.B
  io.variableWriteMemRequest.bits.addr := (bankWriteAddress << variablesMemAddressWidth.U).asUInt + variableWriteAddress
  io.variableWriteMemRequest.bits.dataIn := resultVariableQueue.bits
  io.variableWriteMemRequest.valid := diagonalValueColumnIndexQueue.valid && resultVariableQueue.valid

  private val writeReady = io.variableWriteMemRequest.valid && io.variableWriteMemRequest.ready
  diagonalValueColumnIndexQueue.ready := writeReady
  resultVariableQueue.ready := writeReady

  private val variableWriteCountReg = Reg(UInt((exp + sign).W))
  when(io.variableWriteMemRequest.valid && io.variableWriteMemRequest.ready) {

    variableWriteCountReg := variableWriteCountReg + 1.U
  }.otherwise {

    variableWriteCountReg := variableWriteCountReg
  }

  when(busyReg && variableWriteCountReg === rowsQueue.bits) {

    busyReg := false.B
    variableWriteCountReg := 0.U
    rowsQueue.ready := true.B
  }.elsewhen(busyReg && io.variableWriteMemRequest.valid && io.variableWriteMemRequest.ready) {

    busyReg := true.B
    variableWriteCountReg + 1.U
    rowsQueue.ready := false.B
  }.otherwise {

    busyReg := Mux(!busyReg && rowsQueue.valid, true.B, busyReg)
    variableWriteCountReg := variableWriteCountReg
    rowsQueue.ready := false.B
  }

  io.variableOut <> io.variableIn

  io.busy := busyReg

  def connect(addressGenerator: SyGSAddressGenerator, index: Int): Unit = {

    addressGenerator.io.variablesMemRequests(index)(0) <> io.variableWriteMemRequest
    addressGenerator.io.variablesMemRequests(index)(1) <> io.variableReadMemRequest

    io.variableIn <> addressGenerator.io.variablesMemResponses(index)

    io.resultVariableIn.bits := addressGenerator.io.dataFlowEngineIO.out.bits.bits
    io.resultVariableIn.valid := addressGenerator.io.dataFlowEngineIO.out.valid && addressGenerator.io.dataFlowEngineIO.producerIndex === index.U
    addressGenerator.io.dataFlowEngineIO.out.ready := io.resultVariableIn.ready

    addressGenerator.io.dataFlowEngineIO.variables(index).bits.bits := io.variableOut.bits
    addressGenerator.io.dataFlowEngineIO.variables(index).valid := io.variableOut.valid
    io.variableOut.ready := addressGenerator.io.dataFlowEngineIO.variables(index).ready
  }
}
