package spatial_templates

import chisel3._

class RoCCInstruction extends Bundle {
  val funct = Bits(width = 7.W)
  val rs2 = Bits(width = 5.W)
  val rs1 = Bits(width = 5.W)
  val xd = Bool()
  val xs1 = Bool()
  val xs2 = Bool()
  val rd = Bits(width = 5.W)
  val opcode = Bits(width = 7.W)
}

/**
  * Generic controller interface to communicate with PEs in a template. Contains a single 'done'
  * boolean signal for each PE and a decoupled producer interface to issue commands
  *
  * @param n the number of PEs this interface is responsible for
  */
class ControllerIO(n: Int) extends Bundle {
  val pe_cmd = Output(Bits(14.W))
  val pe_cmd_valid = Output(Vec(n, Bool()))
  val pe_idle = Input(Vec(n, Bool()))
}

/**
  * Decomposition of the RoCC interface meant to be used by a PE controller.
  * It allows the controller to act as handler for the RoCC command. It excludes
  * the memory interface handling since this should be handled by a separate component
  *
  */
class PERoCCIO(xLen: Int = 64) extends Bundle {
  val cmd_rdy = Output(Bool())
  val cmd_valid = Input(Bool())
  val cmd_inst = Input(new RoCCInstruction())
  val cmd_rs1 = Input(Bits(xLen.W))
  val cmd_rs2 = Input(Bits(xLen.W))
  //val cmd_status = Input(new MStatus)
  val resp_data = Output(Bits(xLen.W))
  val resp_rdy = Input(Bool())
  val resp_valid = Output(Bool())
  val busy = Output(Bool())
  val interrupt = Output(Bool())
}

/**
  * Bundle of a PE contoller handling RoCC commands
  *
  * @param n number of PEs for this controller
  */
class RoCCControllerIO(n: Int, xLen: Int = 32) extends Bundle {
  val rocc_cmd = new PERoCCIO(xLen)
}

class RoCCPEController(n: Int) extends MultiIOModule {
  val io = IO(new RoCCControllerIO(n))
}
