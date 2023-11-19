package sygs.pe.ag.submodules

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.ag._

/**
  * This module sends requests to the local memory interface to get the row values and column indices
  * needed by the Data Flow Engine and the VariableMemRequestGenerator respectively.
  * It waits for informations sent by the MetadataMemRequestGenerator to start collecting data
  * for the current row.
  */
class SyGSDataMemReqGen(exp:Int,
                        sign: Int,
                        accumulatorMemAddressWidth: Int,
                        metadataQueueSize: Int) extends Module {

  val io = IO(new Bundle() {

    val backwards = Input(Bool())

    val offset = Input(Valid(UInt((exp + sign).W)))
    val totalNZ = Flipped(Decoupled(UInt((exp + sign).W)))

    val numberOfNonZerosIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val diagonalValueIndexIn = Flipped(Decoupled(UInt((exp + sign).W)))

    val columnIndexIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val rowValueIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val diagonalValueIn = Flipped(Decoupled(UInt((exp + sign).W)))
    val diagonalValueColumnIndexIn = Flipped(Decoupled(UInt((exp + sign).W)))

    val columnIndexMemRequest = Decoupled(new MemRequestIO(exp + sign, accumulatorMemAddressWidth))
    val diagonalValueColumnIndexMemRequest = Decoupled(new MemRequestIO(exp + sign, accumulatorMemAddressWidth))
    val rowValueMemRequest = Decoupled(new MemRequestIO(exp + sign, accumulatorMemAddressWidth))
    val diagonalValueMemRequest = Decoupled(new MemRequestIO(exp + sign, accumulatorMemAddressWidth))

    val columnIndexOut = Decoupled(UInt((exp + sign).W))
    val rowValueOut = Decoupled(UInt((exp + sign).W))
    val diagonalValueOut = Decoupled(UInt((exp + sign).W))
    val diagonalValueColumnIndexOut = Decoupled(UInt((exp + sign).W))

    val busy = Output(Bool())
  })

  private val busyReg = Reg(Bool())
  private val offsetReg = Reg(Valid(UInt((exp + sign).W)))

  private val totalNZQueue = Queue(io.totalNZ, 1)
  private val numberOfNonZerosQueue = Queue(io.numberOfNonZerosIn, metadataQueueSize)
  private val diagonalValueIndexQueue = Queue(io.diagonalValueIndexIn, metadataQueueSize)

  private val diagonalValueIndex = Mux(io.backwards,
    numberOfNonZerosQueue.bits - diagonalValueIndexQueue.bits - 1.U,
    diagonalValueIndexQueue.bits)

  totalNZQueue.ready := false.B //TODO maybe useless queue

  private val columnIndexReqsDoneReg = Reg(Bool())
  private val diagonalValueColumnIndexReqDoneReg = Reg(Bool())
  private val rowValueReqsDoneReg = Reg(Bool())
  private val diagonalValueReqDoneReg = Reg(Bool())

  private val rowReqsDone = columnIndexReqsDoneReg && diagonalValueColumnIndexReqDoneReg &&
    rowValueReqsDoneReg && diagonalValueReqDoneReg

  private val currentColumnIndexReqReg = RegInit(UInt((exp + sign).W), 0.U)
  private val currentRowValueReqReg = RegInit(UInt((exp + sign).W), 0.U)

  numberOfNonZerosQueue.ready := rowReqsDone
  diagonalValueIndexQueue.ready := rowReqsDone

  when(busyReg && !numberOfNonZerosQueue.valid) { busyReg := false.B }
    .elsewhen(busyReg && rowReqsDone) { busyReg := true.B }
    .otherwise { busyReg := Mux(!busyReg && numberOfNonZerosQueue.valid, true.B, busyReg) }

  private val offsetReadReg = RegInit(Bool(), false.B)
  when(io.offset.valid && !offsetReadReg) {

    offsetReg.bits := io.offset.bits
    offsetReg.valid := !io.backwards
    offsetReadReg := true.B
  }.elsewhen(totalNZQueue.valid && !offsetReg.valid) {

    offsetReg.bits := offsetReg.bits + totalNZQueue.bits - 1.U
    offsetReg.valid := true.B
  }.elsewhen(rowReqsDone) {

    offsetReg.bits := Mux(io.backwards, offsetReg.bits - numberOfNonZerosQueue.bits, offsetReg.bits + numberOfNonZerosQueue.bits)
  }.otherwise {

    offsetReg.bits := offsetReg.bits
  }

  Seq(io.columnIndexMemRequest, io.diagonalValueColumnIndexMemRequest, io.rowValueMemRequest, io.diagonalValueMemRequest)
    .foreach { req =>

      req.bits.write := false.B
      req.bits.dataIn := 0.U
    }

  private val canStartReqs = busyReg && numberOfNonZerosQueue.valid && diagonalValueIndexQueue.valid && offsetReg.valid

  io.columnIndexMemRequest.bits.addr := Mux(io.backwards,
    offsetReg.bits + totalNZQueue.bits - currentColumnIndexReqReg,
    offsetReg.bits + totalNZQueue.bits + currentColumnIndexReqReg)
  io.columnIndexMemRequest.valid := canStartReqs && io.columnIndexIn.ready && !columnIndexReqsDoneReg &&
    totalNZQueue.valid && currentColumnIndexReqReg =/= diagonalValueIndex

  io.diagonalValueColumnIndexMemRequest.bits.addr := Mux(io.backwards,
    offsetReg.bits + totalNZQueue.bits - diagonalValueIndex,
    offsetReg.bits + totalNZQueue.bits + diagonalValueIndex)
  io.diagonalValueColumnIndexMemRequest.valid := canStartReqs && io.diagonalValueColumnIndexIn.ready &&
    !diagonalValueColumnIndexReqDoneReg && totalNZQueue.valid

  io.rowValueMemRequest.bits.addr := Mux(io.backwards,
    offsetReg.bits - currentRowValueReqReg,
    offsetReg.bits + currentRowValueReqReg)
  io.rowValueMemRequest.valid := canStartReqs && io.rowValueOut.ready && !rowValueReqsDoneReg &&
    currentRowValueReqReg =/= diagonalValueIndex

  io.diagonalValueMemRequest.bits.addr := Mux(io.backwards,
    offsetReg.bits - diagonalValueIndex,
    offsetReg.bits + diagonalValueIndex)
  io.diagonalValueMemRequest.valid := canStartReqs && io.diagonalValueOut.ready && !diagonalValueReqDoneReg

  Seq((currentColumnIndexReqReg, io.columnIndexMemRequest, columnIndexReqsDoneReg),
    (currentRowValueReqReg, io.rowValueMemRequest, rowValueReqsDoneReg))
    .foreach { case (current, req, done) =>
      when(busyReg && !done) {

        when(current + 1.U < numberOfNonZerosQueue.bits && ((req.valid && req.ready) ||
          diagonalValueIndexQueue.valid && current === diagonalValueIndex)) {

          when(current + 1.U =/= diagonalValueIndex) {

            current := current + 1.U
            done := false.B
          }.elsewhen(current + 2.U < numberOfNonZerosQueue.bits) {

            current := current + 2.U
            done := false.B
          }.otherwise {

            current := 0.U
            done := true.B
          }
        }.elsewhen((req.valid && req.ready) ||
          diagonalValueIndexQueue.valid && current === diagonalValueIndex) {

          current := 0.U
          done := true.B
        }.otherwise {

          current := current
          done := done
        }
      }.elsewhen(busyReg && rowReqsDone) {

        done := false.B
      }.otherwise {

        done := done
      }
    }

  Seq((io.diagonalValueMemRequest, diagonalValueReqDoneReg),
    (io.diagonalValueColumnIndexMemRequest, diagonalValueColumnIndexReqDoneReg)).foreach { case (req, done) =>

    when(!done) { done := req.valid && req.ready }
      .elsewhen(rowReqsDone) { done := false.B }
      .otherwise { done := done }
  }

  io.columnIndexOut <> io.columnIndexIn
  io.rowValueOut <> io.rowValueIn
  io.diagonalValueOut <> io.diagonalValueIn
  io.diagonalValueColumnIndexOut <> io.diagonalValueColumnIndexIn

  io.busy := busyReg

  def connect(addressGenerator: SyGSAddressGenerator, index: Int): Unit = {

    addressGenerator.io.accumulatorMemRequests(index)(0) <> io.columnIndexMemRequest
    addressGenerator.io.accumulatorMemRequests(index)(3) <> io.diagonalValueColumnIndexMemRequest
    addressGenerator.io.accumulatorMemRequests(index)(4) <> io.diagonalValueMemRequest
    addressGenerator.io.accumulatorMemRequests(index)(5) <> io.rowValueMemRequest

    io.columnIndexIn <> addressGenerator.io.accumulatorMemResponses(index)(0)
    io.diagonalValueColumnIndexIn <> addressGenerator.io.accumulatorMemResponses(index)(3)
    io.diagonalValueIn <> addressGenerator.io.accumulatorMemResponses(index)(4)
    io.rowValueIn <> addressGenerator.io.accumulatorMemResponses(index)(5)

    addressGenerator.io.dataFlowEngineIO.rowValues(index).bits.bits := io.rowValueOut.bits
    addressGenerator.io.dataFlowEngineIO.rowValues(index).valid := io.rowValueOut.valid
    io.rowValueOut.ready := addressGenerator.io.dataFlowEngineIO.rowValues(index).ready

    addressGenerator.io.dataFlowEngineIO.diagonalValues(index).bits.bits := io.diagonalValueOut.bits
    addressGenerator.io.dataFlowEngineIO.diagonalValues(index).valid := io.diagonalValueOut.valid
    io.diagonalValueOut.ready := addressGenerator.io.dataFlowEngineIO.diagonalValues(index).ready
  }

  def connect(variableMemRequestGenerator: SyGSVariableMemReqGen): Unit = {

    variableMemRequestGenerator.io.columnIndexIn <> io.columnIndexOut
    variableMemRequestGenerator.io.diagonalValueColumnIndexIn <> io.diagonalValueColumnIndexOut
  }
}
