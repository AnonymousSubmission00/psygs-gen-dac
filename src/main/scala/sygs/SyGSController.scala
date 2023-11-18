package sygs

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe._

/*
SyGS INSTRUCTION INTERFACE

| FUNCT | RS1 | RS2 | XD | XS1 | XS2 | RD | OPCODE |
| 0 6 | 7 11 | 12 16 | 17 | 18 | 19 | 20 24 | 25 31 |
WRITE_REQUEST
| mem_interface_id | addr | - | 1 | 1 | 0 | ack_reg | CUSTOM_0 |
WRITE_MEM
| - | most significant half | less significant half | 0 | 1 | 1 | ack_reg | CUSTOM_1 |
READ_MEM
| mem_interface_id | addr | word half (32bit of 64bit word: 0 first half, 1 second half) | 1 | 1 | 1 | res | CUSTOM_2 |
EXECUTE
| backwards | - | - | 1 | 0 | 0 | ack_reg | CUSTOM_3 |

---------------------------------------------------------

mem_interface_id:
  0: variables mem interface
  1 - N: accumulator mem interfaces

where N = total number of accumulators among all PEs
*/

object SyGSOpCode extends Enumeration {
  val WREQ = Value(11)
  val WMEM = Value(43)
  val RMEM = Value(91)
  val EXEC = Value(123)
}

class SyGSControllerMemIO(addr_width: Int, p_width: Int) extends Bundle {
  val request = Decoupled(new MemRequestIO(p_width, addr_width))
  val response = Flipped(Decoupled(UInt(p_width.W)))
  val busy = Input(Bool())
}

class SyGSCommand(xLen: Int = 32) extends Bundle {
  val inst = new RoCCInstruction()
  val rs1 = Bits(xLen.W)
  val rs2 = Bits(xLen.W)
}

/**
  * This module handles the communication between our template and RocketChip through RoCCCommands.
  * According to the command taken, it can read or write on the BRAMs and can start the computation
  * of our architecture.
  * It connects together all the Processing Elements to coordinate the computations.
  */
class SyGSController(
                      numberOfPEs: Int,
                      accumulatorsPerPE: Int,
                      addr_width: Int,
                      dataWidth: Int
                    ) extends RoCCPEController(numberOfPEs) {

  val pe_ctrl_IOs = IO(Flipped(Vec(numberOfPEs, new CtrlInterfaceIO())))

  pe_ctrl_IOs.foreach{ pe =>

    pe.ctrl_cmd.valid := false.B
    pe.ctrl_cmd.bits := DontCare
  }

  val pe_coordination_IOs = IO(Flipped(Vec(numberOfPEs, new CoordinationIO())))

  val mem_IOs = IO(Vec(numberOfPEs * accumulatorsPerPE + 1, new SyGSControllerMemIO(addr_width, dataWidth)))
  setAllReqsInvalid()

  private val canProceed = Reg(Bool())
  when(!(pe_coordination_IOs.map(io => io.computing).reduce(_ || _)/* || mem_IOs.map( _.busy).reduce(_ | _)*/)) { canProceed := true.B }
    .elsewhen(pe_coordination_IOs.map(io => io.computing).reduce(_ && _)) { canProceed := false.B }
    .otherwise { canProceed := canProceed }
  pe_coordination_IOs.foreach { io => io.canProceed := canProceed }

  private val cmdIn = Wire(Decoupled(new SyGSCommand()))
  private val respOut = Wire(Decoupled(UInt(dataWidth.W)))
  mem_IOs.foreach(_.response.ready := respOut.ready)

  private val cmdQueue = Queue(cmdIn, 2)
  private val respQueue = Queue(respOut, 2)

  // Hold response for 1 cycle in case of non-memory operation
  private val respReg = Reg(Valid(UInt()))

  private val writeRequest = Reg(Valid(new Bundle() {

    val address = UInt()
    val memInterfaceId = UInt()
  }))
  private val secondHalfRead = Reg(Bool())
  private val readResult = Reg(Valid(UInt(dataWidth.W)))

  private val lastOpCode = RegInit(0.U(13.W))
  private val cycles = RegInit(0.U(32.W))
  private val executing = Reg(Bool())

  //input command
  io.rocc_cmd.cmd_rdy := cmdIn.ready && !executing
  cmdIn.bits.inst := io.rocc_cmd.cmd_inst
  cmdIn.bits.rs1 := io.rocc_cmd.cmd_rs1
  cmdIn.bits.rs2 := io.rocc_cmd.cmd_rs2
  cmdIn.valid := io.rocc_cmd.cmd_valid

  // Connect dequeue response
  io.rocc_cmd.resp_data := respQueue.bits
  io.rocc_cmd.resp_valid := respQueue.valid
  respQueue.ready := io.rocc_cmd.resp_rdy

  //busy and interrupt
  io.rocc_cmd.busy := !pe_ctrl_IOs.map( _.idle).reduce(_ & _) || mem_IOs.map( _.busy).reduce(_ | _)
  io.rocc_cmd.interrupt := false.B

  private val responseArbiter = Module(new Arbiter(UInt(), mem_IOs.length))
  for((io, i) <- mem_IOs.zipWithIndex)
    responseArbiter.io.in(i) <> io.response

  responseArbiter.io.out.ready := respOut.ready

  //connect respOut bits
  when(mem_IOs.map( _.response.valid).reduce(_ | _)) {

    respOut.bits := responseArbiter.io.out.bits >> 32
    respOut.valid := responseArbiter.io.out.valid

    readResult.bits := responseArbiter.io.out.bits
    readResult.valid := responseArbiter.io.out.valid
  }.elsewhen(readResult.valid && secondHalfRead) {

    respOut.bits := readResult.bits
    respOut.valid := readResult.valid

    readResult.valid := false.B
    secondHalfRead := false.B
  }.otherwise {

    respOut.bits := respReg.bits
    respOut.valid := respReg.valid
  }

  //when req ready to be executed
  cmdQueue.ready := pe_ctrl_IOs.map( _.idle).reduce(_ & _) && mem_IOs.map( _.request.ready).reduce(_ & _)

  // Count exec cycles
  when(executing) {
    cycles := cycles + 1.U
  }

  // When PEs are no longer busy and we have been executing at leas one cycle,
  // set executing reg to false and send the valid response
  when(pe_ctrl_IOs.map( _.idle).reduce(_ & _) && RegNext(executing)) { executing := false.B }

  //what to do with each request
  when(cmdQueue.bits.inst.opcode === SyGSOpCode.EXEC.id.U && cmdQueue.valid && cmdQueue.ready) {

    pe_ctrl_IOs.foreach { pe =>

      pe.ctrl_cmd.bits := cmdQueue.bits.inst.funct
      pe.ctrl_cmd.valid := true.B
    }
    executing := true.B
    cycles := 0.U

    // Pipe ack response
    respReg.bits := 0.U
    respReg.valid := false.B
    lastOpCode := SyGSOpCode.EXEC.id.U
  }.elsewhen(!executing && lastOpCode === SyGSOpCode.EXEC.id.U) {

    setAllReqsInvalid() //TODO remove?

    // Pipe ack response
    respReg.bits := cycles
    respReg.valid := true.B
    lastOpCode := 0.U
  }.elsewhen(cmdQueue.bits.inst.opcode === SyGSOpCode.WREQ.id.U && cmdQueue.valid && cmdQueue.ready) {

    writeRequest.valid := true.B
    writeRequest.bits.memInterfaceId := cmdQueue.bits.inst.funct
    writeRequest.bits.address := cmdQueue.bits.rs1

    // Pipe ack response
    respReg.bits := 1.U
    respReg.valid := true.B
    lastOpCode := SyGSOpCode.WREQ.id.U
  }.elsewhen(cmdQueue.bits.inst.opcode === SyGSOpCode.WMEM.id.U && cmdQueue.valid && cmdQueue.ready) {

    setAllReqsInvalid()

    when(writeRequest.valid) {

      val dataIn = Wire(UInt(dataWidth.W))
      dataIn := (cmdQueue.bits.rs1 << 32).asUInt + cmdQueue.bits.rs2

      mem_IOs(writeRequest.bits.memInterfaceId).request.bits.write := true.B
      mem_IOs(writeRequest.bits.memInterfaceId).request.bits.addr := writeRequest.bits.address
      mem_IOs(writeRequest.bits.memInterfaceId).request.bits.dataIn := dataIn
      mem_IOs(writeRequest.bits.memInterfaceId).request.valid := true.B

      writeRequest.valid := false.B
      respReg.bits := 2.U
    }.otherwise { respReg.bits := 0.U }

    // Pipe ack response
    respReg.valid := true.B
    lastOpCode := SyGSOpCode.WMEM.id.U
  }.elsewhen(cmdQueue.bits.inst.opcode === SyGSOpCode.RMEM.id.U && cmdQueue.valid && cmdQueue.ready) {

    setAllReqsInvalid()

    mem_IOs(cmdQueue.bits.inst.funct).request.bits.write := false.B
    mem_IOs(cmdQueue.bits.inst.funct).request.bits.addr := cmdQueue.bits.rs1
    mem_IOs(cmdQueue.bits.inst.funct).request.bits.dataIn := 0.U
    mem_IOs(cmdQueue.bits.inst.funct).request.valid := cmdQueue.bits.rs2 === 0.U //request sent only for the most significant word half

    secondHalfRead := cmdQueue.bits.rs2 =/= 0.U

    // Ack response is sent next cycle by memory
    respReg.bits := 3.U
    respReg.valid := false.B
    lastOpCode := SyGSOpCode.RMEM.id.U
  }.otherwise {

    setAllReqsInvalid()
    // No valid instruction this cycle
    // Pipe ack response
    respReg.bits := 0.U
    respReg.valid := false.B
    lastOpCode := lastOpCode
  }

  private def setAllReqsInvalid(): Unit = {

    mem_IOs.foreach { m =>
      m.request.bits.write := false.B
      m.request.bits.addr := DontCare
      m.request.bits.dataIn := DontCare
      m.request.valid := false.B
    }
  }

  def connectPEs(pes: Seq[SyGSPE]): Unit = {

    for((pe, i) <- pes.zipWithIndex) {

      pe.io <> pe_ctrl_IOs(i)
      pe.coordination_io <> pe_coordination_IOs(i)
    }
  }

  def connectMemInterface(interface: NtoMMemInterface, index: Int): Unit = {

    interface.io.inReq(interface.io.inReq.length - 1) <> mem_IOs(index).request
    mem_IOs(index).response <> interface.io.outData(interface.io.inReq.length - 1)
    mem_IOs(index).busy := interface.io.busy
  }
}
