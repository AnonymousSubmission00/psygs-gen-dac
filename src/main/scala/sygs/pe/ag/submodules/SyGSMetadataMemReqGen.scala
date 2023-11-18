package sygs
package pe.ag.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.ag.SyGSAddressGenerator

/**
  * This module sends requests to the local memory interface to get Metadata informations,
  * they're then forwarded to the other components of the Address Generator and to the Data Flow Engine.
  * It keeps track of the rows done to coordinate different PEs
  *
  * start: command sent by the controller to start the execution
  *
  * canProceed: boolean to coordinate with the other PEs
  *
  * backward: boolean to indicate if it's the forward or backard iteration
  *
  * totalsMemRequest, totalsMemResponse, CRMemRequest, CRMemResponse, memRequests, memResponses
  * are all different channels to send requests and get responses of different informations
  *
  * dataOffset, rows, out are channels to send information to other modules (DataMemRequestGenerator, VariableMemRequestGenerator)
  *
  * computing: boolean that say when the module is actively requesting information to memory for a color
  *
  * busy: boolean to indicate when the module has finished all the colors (therefore one complete iteration)
  */

class SyGSMetadataMemReqGen(exp: Int, sign: Int, memAddressWidth: Int) extends Module {

  val io = IO(new Bundle() {

    val start = Input(Bool())
    val canProceed = Input(Bool())
    val backwards = Input(Bool())

    val numberOfNonZerosReady = Input(Bool())

    val totalsMemRequest = Decoupled(new MemRequestIO(exp + sign, memAddressWidth))
    val totalsMemResponse = Flipped(Decoupled(UInt((exp + sign).W)))
    val totalNZ = Decoupled(UInt((exp + sign).W))
    val dataOffset = Output(Valid(UInt((exp + sign).W)))

    val CRMemRequest = Decoupled(new MemRequestIO(exp + sign, memAddressWidth))
    val CRMemResponse = Flipped(Decoupled(UInt((exp + sign).W)))
    val rows = Decoupled(UInt((exp + sign).W))

    //in order: numberOfNonZeros, diagonalValueIndex, initSum
    val memRequests = Vec(3, Decoupled(new MemRequestIO(exp + sign, memAddressWidth)))
    val memResponses = Vec(3, Flipped(Decoupled(UInt((exp + sign).W))))
    val out = Vec(3, Decoupled(UInt((exp + sign).W)))

    val computing = Output(Bool())
    val busy = Output(Bool())
  })

  private val colorOffsetReg = RegInit(UInt((exp + sign).W), 3.U)
  private val rowOffsetReg = RegInit(UInt((exp + sign).W), 3.U)
  private val colorReqDoneReg = Reg(Bool())
  private val rowReqDoneReg = Reg(Bool())
  private val totalRowsReqDoneReg = Reg(Bool())
  private val totalNZReqDoneReg = Reg(Bool())
  totalNZReqDoneReg := totalNZReqDoneReg

  private val colorResponse = Wire(Decoupled(UInt((exp + sign).W)))
  private val rowResponse = Wire(Decoupled(UInt((exp + sign).W)))
  private val totalRowsResponse = Wire(Decoupled(UInt((exp + sign).W)))

  private val waitingColorReg = Reg(Bool())
  when(!colorReqDoneReg && io.CRMemRequest.ready && io.CRMemRequest.valid) { waitingColorReg := true.B }
    .elsewhen(colorResponse.valid) { waitingColorReg := false.B }
    .otherwise { waitingColorReg := waitingColorReg }

  private val waitingTotalRowsReg = Reg(Bool())
  when(!totalRowsReqDoneReg && io.totalsMemRequest.ready && io.totalsMemRequest.valid) { waitingTotalRowsReg := true.B }
    .elsewhen(colorResponse.valid) { waitingTotalRowsReg := false.B }
    .otherwise { waitingTotalRowsReg := waitingTotalRowsReg }

  colorResponse <> io.CRMemResponse
  rowResponse <> io.CRMemResponse
  totalRowsResponse <> io.totalsMemResponse
  io.totalNZ <> io.totalsMemResponse

  colorResponse.valid := waitingColorReg && io.CRMemResponse.valid
  rowResponse.valid := !waitingColorReg && io.CRMemResponse.valid
  totalRowsResponse.valid := waitingTotalRowsReg && io.totalsMemResponse.valid
  io.totalNZ.valid := !waitingTotalRowsReg && io.totalsMemResponse.valid

  private val colorQueue = Queue(colorResponse, 1)
  private val rowsQueue = Queue(rowResponse, 1)
  private val totalRowsQueue = Queue(totalRowsResponse, 1)
  totalRowsQueue.ready := false.B

  io.dataOffset.bits := colorQueue.bits + 3.U * (totalRowsQueue.bits + 1.U)
  io.dataOffset.valid := colorQueue.valid && totalRowsQueue.valid

  private val busyReg = Reg(Bool())
  private val computingReg = Reg(Bool())
  private val rowsDoneReg = Reg(UInt((exp + sign).W))
  private val colorsDoneReg = Reg(UInt((exp + sign).W))
  private val reqsDoneReg = Reg(Vec(3, Bool()))
  private val allReqsDone = reqsDoneReg.reduce(_ && _)

  private val numberOfNonZerosQueue = Queue(io.memResponses(0), 1)
  numberOfNonZerosQueue.ready := allReqsDone

  private val colorDone = (allReqsDone && rowsDoneReg === rowsQueue.bits - 1.U) || rowsQueue.bits === 0.U

  when(io.canProceed && colorQueue.valid && rowsQueue.valid && (!io.backwards || totalRowsQueue.valid)) { computingReg := true.B }
    .elsewhen(colorDone) { computingReg := false.B }
    .otherwise { computingReg := computingReg }

  //colorOffsetReg
  when(io.CRMemResponse.valid && waitingColorReg) {

    colorOffsetReg := 3.U + Mux(io.backwards, io.CRMemResponse.bits - 1.U, 0.U)
  }.elsewhen(computingReg && colorDone) {

    colorOffsetReg := Mux(io.backwards, colorOffsetReg - 1.U, colorOffsetReg + 1.U)
  }.otherwise {

    colorOffsetReg := colorOffsetReg
  }

  //rowOffsetReg
  when(io.CRMemResponse.valid && waitingColorReg && io.totalsMemResponse.valid && waitingTotalRowsReg) {

    rowOffsetReg := 3.U + io.CRMemResponse.bits + Mux(io.backwards, (io.totalsMemResponse.bits - 1.U) * 3.U, 0.U)
  }.elsewhen(io.CRMemResponse.valid && waitingColorReg) {

    rowOffsetReg := rowOffsetReg + io.CRMemResponse.bits
  }.elsewhen(io.totalsMemResponse.valid && waitingTotalRowsReg) {

    rowOffsetReg := rowOffsetReg + Mux(io.backwards, (io.totalsMemResponse.bits - 1.U) * 3.U, 0.U)
  }.elsewhen(computingReg && allReqsDone) {

    rowOffsetReg := Mux(io.backwards, rowOffsetReg - 3.U, rowOffsetReg + 3.U)
  }.otherwise {

    rowOffsetReg := rowOffsetReg
  }

  when(computingReg && colorDone) {

    rowsDoneReg := 0.U
    rowsQueue.ready := true.B
  }.elsewhen(computingReg && allReqsDone) {

    rowsDoneReg := rowsDoneReg + 1.U
    rowsQueue.ready := false.B
  }.otherwise {

    rowsDoneReg := rowsDoneReg
    rowsQueue.ready := false.B
  }

  when(computingReg && colorDone && colorsDoneReg === colorQueue.bits - 1.U || (colorQueue.bits === 0.U && colorQueue.valid)) {

    busyReg := false.B
    colorsDoneReg := 0.U
    //colorOffsetReg := 3.U
    colorQueue.ready := true.B
    totalNZReqDoneReg := false.B
  }.elsewhen(computingReg && colorDone) {

    busyReg := true.B
    colorsDoneReg := colorsDoneReg + 1.U
    colorQueue.ready := false.B
  }.otherwise {

    busyReg := Mux(busyReg || io.start, true.B, busyReg)
    colorsDoneReg := colorsDoneReg
    colorQueue.ready := false.B
  }

  io.CRMemRequest.bits.write := false.B
  io.CRMemRequest.bits.dataIn := 0.U
  when(!colorReqDoneReg) {

    io.CRMemRequest.bits.addr := 2.U
    io.CRMemRequest.valid := (io.start || busyReg)
  }.otherwise {

    io.CRMemRequest.bits.addr := colorOffsetReg
    io.CRMemRequest.valid := busyReg && !rowReqDoneReg && colorQueue.valid
  }

  io.totalsMemRequest.bits.write := false.B
  io.totalsMemRequest.bits.dataIn := 0.U
  when(!totalRowsReqDoneReg) {

    io.totalsMemRequest.bits.addr := 1.U
    io.totalsMemRequest.valid := (io.start || busyReg)
  }.otherwise {

    io.totalsMemRequest.bits.addr := 0.U
    io.totalsMemRequest.valid := busyReg && !totalNZReqDoneReg
  }

  when(!colorReqDoneReg) { colorReqDoneReg := io.CRMemRequest.valid && io.CRMemRequest.ready }
    .elsewhen(!busyReg) { colorReqDoneReg := false.B }
    .otherwise { colorReqDoneReg := colorReqDoneReg }

  when(!rowReqDoneReg) { rowReqDoneReg := colorReqDoneReg && io.CRMemRequest.valid && io.CRMemRequest.ready }
    .elsewhen(rowsQueue.ready) { rowReqDoneReg := false.B }
    .otherwise { rowReqDoneReg := rowReqDoneReg }

  when(!totalRowsReqDoneReg) { totalRowsReqDoneReg := io.totalsMemRequest.valid && io.totalsMemRequest.ready }
    .elsewhen(!busyReg) { totalRowsReqDoneReg := false.B }
    .otherwise { totalRowsReqDoneReg := totalRowsReqDoneReg }

  when(!totalNZReqDoneReg) { totalNZReqDoneReg := totalRowsReqDoneReg && io.totalsMemRequest.valid && io.totalsMemRequest.ready }

  io.rows.bits := rowsQueue.bits
  io.rows.valid := computingReg && rowsQueue.valid && rowsQueue.bits =/= 0.U

  for(i <- 0 until 3) {

    val req = io.memRequests(i)
    req.bits.addr := rowOffsetReg + i.U
    req.bits.write := false.B
    req.bits.dataIn := 0.U
    req.valid := computingReg && !reqsDoneReg(i) && io.out(i).ready && rowsQueue.bits =/= 0.U

    val reqDone = reqsDoneReg(i)
    when(!reqDone) { reqDone := req.valid && req.ready }
      .elsewhen(allReqsDone) { reqDone := false.B }
      .otherwise { reqDone := reqDone }

    io.out(i) <> io.memResponses(i)
  }

  io.out(0).valid := io.memResponses(0).valid && io.out(0).ready

  io.computing := computingReg
  io.busy := busyReg

  def connect(addressGenerator: SyGSAddressGenerator, index: Int): Unit = {

    io.canProceed := !addressGenerator.io.computing && addressGenerator.io.canProceed
    io.backwards := addressGenerator.io.backwards

    addressGenerator.io.accumulatorMemRequests(index)(1) <> io.memRequests(0)
    addressGenerator.io.accumulatorMemRequests(index)(2) <> io.memRequests(1)
    addressGenerator.io.accumulatorMemRequests(index)(6) <> io.memRequests(2)
    addressGenerator.io.accumulatorMemRequests(index)(7) <> io.totalsMemRequest
    addressGenerator.io.accumulatorMemRequests(index)(8) <> io.CRMemRequest

    io.memResponses(0) <> addressGenerator.io.accumulatorMemResponses(index)(1)
    io.memResponses(1) <> addressGenerator.io.accumulatorMemResponses(index)(2)
    io.memResponses(2) <> addressGenerator.io.accumulatorMemResponses(index)(6)
    io.totalsMemResponse <> addressGenerator.io.accumulatorMemResponses(index)(7)
    io.CRMemResponse <> addressGenerator.io.accumulatorMemResponses(index)(8)

    addressGenerator.io.dataFlowEngineIO.numbersOfNonZeros(index).bits := io.out(0).bits
    addressGenerator.io.dataFlowEngineIO.initSums(index).bits.bits := io.out(2).bits

    addressGenerator.io.dataFlowEngineIO.numbersOfNonZeros(index).valid := io.out(0).valid
    addressGenerator.io.dataFlowEngineIO.initSums(index).valid := io.out(2).valid

    io.out(0).ready := io.numberOfNonZerosReady && addressGenerator.io.dataFlowEngineIO.numbersOfNonZeros(index).ready
    io.out(2).ready := addressGenerator.io.dataFlowEngineIO.initSums(index).ready

    io.start := addressGenerator.io.start
  }

  def connect(dataMemRequestGenerator: SyGSDataMemReqGen, variableMemRequestGenerator: SyGSVariableMemReqGen): Unit = {

    dataMemRequestGenerator.io.backwards := io.backwards
    dataMemRequestGenerator.io.offset := io.dataOffset
    dataMemRequestGenerator.io.totalNZ <> io.totalNZ

    variableMemRequestGenerator.io.rows <> io.rows
    io.rows.ready := variableMemRequestGenerator.io.rows.ready

    dataMemRequestGenerator.io.diagonalValueIndexIn <> io.out(1)

    dataMemRequestGenerator.io.numberOfNonZerosIn.bits := io.out(0).bits
    dataMemRequestGenerator.io.numberOfNonZerosIn.valid := io.out(0).valid

    io.numberOfNonZerosReady := dataMemRequestGenerator.io.numberOfNonZerosIn.ready
  }
}
