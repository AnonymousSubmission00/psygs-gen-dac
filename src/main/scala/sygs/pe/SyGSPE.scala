package sygs
package pe

import chisel3._
import chisel3.util._
import spatial_templates._
import sygs.pe.ag.SyGSAddressGenerator
import sygs.pe.dfe.SyGSDFE

/**
  * This module represents the basic Processing Element.
  * It can be instantiated with multiple accumulators or multiple inverters at choice
  * It contains an Address Generator for each accumulator in the Data Flow Engine
  */
class CoordinationIO extends Bundle() {

  val canProceed = Input(Bool())
  val computing = Output(Bool())
}

class SyGSPE(numberOfAccumulators: Int,
             numberOfInverters: Int,
             variablesMemBanks: Int,
             accumulatorMemAddressWidth: Int,
             variablesMemAddressWidth: Int,
             multiplierQueueSize: Int,
             accumulatorQueueSize: Int,
             useBlackBoxAdder64: Boolean,
             dividerQueueSize: Int,
             metadataQueueSize: Int,
             columnIndicesQueueSize: Int,
             exp: Int,
             sign: Int,
             memRespQsize: Int
            )
  extends DataflowPE(new ElemId(1, 0, 0, 0), new CtrlInterfaceIO(), exp + sign,
    Math.max(accumulatorMemAddressWidth, log2Up(variablesMemBanks) + variablesMemAddressWidth),
    11 * numberOfAccumulators, 10 * numberOfAccumulators, memRespQsize) {

  val coordination_io = IO(new CoordinationIO)

  private val backwardsReg = Reg(Bool())
  when(io.ctrl_cmd.valid) { backwardsReg := io.ctrl_cmd.bits === 1.U }
    .otherwise { backwardsReg := backwardsReg }

  private val addressGenerator = Module(new SyGSAddressGenerator(numberOfAccumulators, variablesMemBanks,
    multiplierQueueSize, accumulatorQueueSize, metadataQueueSize, columnIndicesQueueSize, dividerQueueSize, exp, sign, accumulatorMemAddressWidth, variablesMemAddressWidth))
  addressGenerator.io.start := io.ctrl_cmd.valid
  addressGenerator.io.canProceed := coordination_io.canProceed
  addressGenerator.io.backwards := backwardsReg || (io.ctrl_cmd.bits === 1.U && io.ctrl_cmd.valid)

  private val dataFlowEngine = Module(new SyGSDFE(numberOfAccumulators, numberOfInverters, multiplierQueueSize, accumulatorQueueSize, dividerQueueSize, exp, sign, useBlackBoxAdder64))
  addressGenerator.io.dataFlowEngineIO <> dataFlowEngine.io

  //connect requests and responses
  private val accumulatorReqs = 9
  for(a <- 0 until numberOfAccumulators) {

    df_io.memQueuesIO.outgoingReq(a) <> addressGenerator.io.variablesMemRequests(a)(0)
    df_io.memQueuesIO.outgoingReq(a + numberOfAccumulators) <> addressGenerator.io.variablesMemRequests(a)(1)

    connectToReadQueue(a, addressGenerator.io.variablesMemResponses(a))

    val reqOffset = 2 * numberOfAccumulators + a * accumulatorReqs
    val respOffset = numberOfAccumulators + a * accumulatorReqs
    for(r <- 0 until accumulatorReqs) {

      df_io.memQueuesIO.outgoingReq(reqOffset + r) <> addressGenerator.io.accumulatorMemRequests(a)(r)
      connectToReadQueue(respOffset + r, addressGenerator.io.accumulatorMemResponses(a)(r))
    }
  }

  io.idle := !addressGenerator.io.busy
  io.ctrl_cmd.ready := true.B
  coordination_io.computing := addressGenerator.io.computing

  when(io.idle && !RegNext(io.idle)) {
    addressGenerator.reset := true.B
  }

  def connectToAccumulatorMemInterface(accumulatorMemInterface: NtoMMemInterface, accumulatorIndex: Int): Unit = {

    val reqOffset = 2 * numberOfAccumulators + accumulatorIndex * accumulatorReqs
    val respOffset = numberOfAccumulators + accumulatorIndex * accumulatorReqs
    for(r <- 0 until accumulatorReqs) {

      accumulatorMemInterface.io.inReq(r) <> df_io.memQueuesIO.outgoingReq(reqOffset + r)

      df_io.memQueuesIO.inReadData(respOffset + r).bits := accumulatorMemInterface.io.outData(r).bits
      df_io.memQueuesIO.inReadData(respOffset + r).valid := accumulatorMemInterface.io.outData(r).valid
      accumulatorMemInterface.io.outData(r).ready := df_io.memQueuesIO.readQueuesEnqReady(respOffset + r)
    }
  }

  def connectToVariablesMemInterface(variablesMemInterface: NtoMMemInterface, peIndex: Int, accumulatorIndex: Int): Unit = {

    val numberOfPEs = variablesMemInterface.reqQueues.size / 2 / numberOfAccumulators

    variablesMemInterface.io.inReq(peIndex * numberOfAccumulators + accumulatorIndex) <> df_io.memQueuesIO.outgoingReq(accumulatorIndex)
    variablesMemInterface.io.inReq((numberOfPEs + peIndex) * numberOfAccumulators + accumulatorIndex) <> df_io.memQueuesIO.outgoingReq(numberOfAccumulators + accumulatorIndex)

    val interfaceOut = variablesMemInterface.io.outData((numberOfPEs + peIndex) * numberOfAccumulators + accumulatorIndex)

    df_io.memQueuesIO.inReadData(accumulatorIndex).bits := interfaceOut.bits
    df_io.memQueuesIO.inReadData(accumulatorIndex).valid := interfaceOut.valid
    interfaceOut.ready := df_io.memQueuesIO.readQueuesEnqReady(accumulatorIndex)

    //we don't need a response for write requests
    variablesMemInterface.io.outData(peIndex * numberOfAccumulators + accumulatorIndex).ready := false.B
  }
}
