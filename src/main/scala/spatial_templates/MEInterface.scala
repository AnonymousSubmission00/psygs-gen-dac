package spatial_templates

import chisel3._
import chisel3.util._

abstract class MEInterface() extends Module {}

class PTPBramLikeIO(val p_width: Int, val addr_width: Int) extends Bundle {
  val from_mem = Flipped(new BRAMLikeIO(p_width, addr_width))
  val to_pe = new BRAMLikeIO(p_width, addr_width)
}

/*class PTPBRAMInterface(width: Int, addr_width: Int) extends MEInterface {
  val io = IO(new PTPBramLikeIO(width, addr_width))
  io.from_mem <> io.to_pe

  def connect(pe: PEWithBRAMMem, me: BRAMLikeMem) {
    // Connect memory element to PTP interface
    me.io <> this.io.from_mem
    // Connect interface to the PE memory-facing IO
    this.io.to_pe <> pe.m_io.mem
  }
}*/

class MemPortIO(val p_width: Int, val addr_width: Int) extends Bundle {
  val write = Input(Bool())
  val addr = Input(UInt(addr_width.W))
  val dataIn = Input(UInt(p_width.W))
  val valid = Input(Bool())
  val dataOut = Output(UInt(p_width.W))
}

class PortSelIO(val p_width: Int, val addr_width: Int, n_inputs: Int)
  extends Bundle {
  // Outputs directed to the SRAM ports
  val portOut_1 = Flipped(new MemPortIO(p_width, addr_width))
  val portOut_2 = Flipped(new MemPortIO(p_width, addr_width))
  // Collection of producers that generate read and write requests for the SRAM
  val inSeq = Vec(n_inputs, new MemPortIO(p_width, addr_width))
  // Ack bits for a completed mem request
  val reqDone = Vec(n_inputs, Output(Bool()))
}

class TwoBankSelector(val p_width: Int, val addr_width: Int, n_inputs: Int)
  extends Module {
  val io = IO(new PortSelIO(p_width, addr_width, n_inputs))

  val oHSeq = Reverse(Cat(io.inSeq.map(_.valid)))
  // Priority encoding for the first bank
  val portOneEncoded = PriorityEncoder(oHSeq)
  val oHSeq_2 = (~(1.U << portOneEncoded)) & oHSeq
  val portTwoEncoded = PriorityEncoder(oHSeq_2)

  val portSelector_1_write = PriorityMux(oHSeq, io.inSeq.map(_.write))
  val portSelector_1_addr = PriorityMux(oHSeq, io.inSeq.map(_.addr))
  val portSelector_1_dataIn = PriorityMux(oHSeq, io.inSeq.map(_.dataIn))
  val portSelector_1_valid = PriorityMux(oHSeq, io.inSeq.map(_.valid))

  io.portOut_1.write := portSelector_1_write
  io.portOut_1.addr := portSelector_1_addr
  io.portOut_1.dataIn := portSelector_1_dataIn
  io.portOut_1.valid := portSelector_1_valid

  val portSelector_2_write = PriorityMux(oHSeq_2, io.inSeq.map(_.write))
  val portSelector_2_addr = PriorityMux(oHSeq_2, io.inSeq.map(_.addr))
  val portSelector_2_dataIn = PriorityMux(oHSeq_2, io.inSeq.map(_.dataIn))
  val portSelector_2_valid = PriorityMux(oHSeq_2, io.inSeq.map(_.valid))

  io.portOut_2.write := portSelector_2_write
  io.portOut_2.addr := portSelector_2_addr
  io.portOut_2.dataIn := portSelector_2_dataIn
  io.portOut_2.valid := portSelector_2_valid

  io.inSeq.zipWithIndex.foreach { case (element: MemPortIO, idx) =>
    element.dataOut := Mux(
      RegNext(((portOneEncoded === idx.U(log2Up(n_inputs).W)))),
      io.portOut_1.dataOut,
      io.portOut_2.dataOut
    )
    io.reqDone(idx) := ((portOneEncoded === idx.U(log2Up(n_inputs).W)) ||
      (portTwoEncoded === idx.U(log2Up(n_inputs).W))) && element.valid
  }
}

class MemRequestIO(val p_width: Int, val addr_width: Int) extends Bundle {
  val write = Input(Bool())
  val addr = Input(UInt(addr_width.W))
  val dataIn = Input(UInt(p_width.W))
}

class BankAccessIO(val p_width: Int, val addr_width: Int, n_banks: Int)
  extends Bundle {
  // Output port to each SRAM bank
  val outSeq = Vec(n_banks, Flipped(new MemPortIO(p_width, addr_width)))
  // Input request before bank selction
  val inReq = Flipped(
    Decoupled(new MemRequestIO(p_width, addr_width + log2Up(n_banks)))
  )
  // Ack bits for a completed mem request
  val reqDone = Vec(n_banks, Input(Bool()))
  // Outcome of a memory read request
  val outData = Decoupled(Output(UInt(p_width.W)))

  val empty = Output(Bool())
}

//TODO integrate in spatial templates
class BankAccessQueue(
                       val p_width: Int,
                       val addr_width: Int,
                       n_banks: Int,
                       reqQSize: Int,
                       respQSize: Int = 0
                     ) extends Module {
  val io = IO(new BankAccessIO(p_width, addr_width, n_banks))

  // Queue holding incoming memory requests
  val requestQueue = Queue(io.inReq, reqQSize)
  // Connect each port to the correct bank
  val reqBank =
    requestQueue.deq.addr(addr_width + log2Up(n_banks) - 1, addr_width)
  io.outSeq.zipWithIndex.foreach { case (element, idx) =>
    val validEl = (reqBank === idx.U(log2Up(n_banks).W)) && requestQueue.valid
    element.write := requestQueue.deq.write && validEl
    element.dataIn := requestQueue.deq.dataIn
    element.valid := validEl
    element.addr := requestQueue.deq.addr(addr_width - 1, 0)
  }

  val dataIn = Wire(Decoupled(UInt(p_width.W)))

  val validNext = Wire(Bool())
  // When the request is served, (and it was my request!!) dequeue it
  when((io.reqDone.asUInt.orR) /*&& requestQueue.valid*//* and receiver is ready*/ && ((io.outData.ready || dataIn.ready) | requestQueue.deq.write)) {
    validNext := !requestQueue.deq.write
    requestQueue.ready := true.B
  }.otherwise {
    requestQueue.ready := false.B
    validNext := false.B
  }
  val outSel = Reverse(Cat(io.reqDone)) & UIntToOH(reqBank)

  dataIn.valid := RegNext(validNext)
  dataIn.bits := Mux1H(RegNext(outSel), io.outSeq.map(_.dataOut))
  // Queue holding value of incoming response
  val responseQueue = Queue(dataIn, respQSize)
  io.outData <> responseQueue

  /* Delay valid signal because SRAM output is ready next cycle
  io.outData.valid := RegNext(validNext)
  io.outData.bits := Mux1H(RegNext(outSel), io.outSeq.map(_.dataOut))*/
  io.empty := !requestQueue.valid
}

class NtoMMemInterfaceIO(
                          val p_width: Int,
                          val addr_width: Int,
                          n_banks: Int,
                          n_queue: Int,
                        ) extends Bundle {

  // Input request before bank selction
  val inReq = Vec(
    n_queue,
    Flipped(Decoupled(new MemRequestIO(p_width, addr_width + log2Up(n_banks))))
  )
  // Outcome of a memory read request
  val outData = Vec(n_queue, Decoupled(Output(UInt(p_width.W))))

  // Outputs directed to the SRAM ports
  val portOut_1 = Vec(n_banks, Flipped(new MemPortIO(p_width, addr_width)))
  val portOut_2 = Vec(n_banks, Flipped(new MemPortIO(p_width, addr_width)))

  val busy = Output(Bool())
}

//TODO integrate in spatial templates
/** Interface to connect N memory requests producers to M Synchronous Memory
  * banks with priority-based parallel access
  *
  * @param p_width
  *   memory port width
  * @param addr_width
  *   address width for each memory bank
  * @param n_banks
  *   number of memory banks
  * @param n_queue
  *   number of memory request queues (producers, e.g. PE)
  * @param reqQSize
  *   depth of the memory requests FIFO
  * @param respQSize
  *   depth of the memory responses FIFO.
  *   If = 0, responses are not held
  *   If > 1, responses are held in a queue until the receiver is ready (adds 1 clock cycle latency)
  *   Must be != 1 (it does not ensure responses are held and adds 1 clock cycle latency)
  */
class NtoMMemInterface(
                        val p_width: Int,
                        val addr_width: Int,
                        n_banks: Int,
                        n_queue: Int,
                        reqQSize: Int,
                        respQSize: Int = 0
                      ) extends MEInterface {

  assert(respQSize != 1)

  val io = IO(new NtoMMemInterfaceIO(p_width, addr_width, n_banks, n_queue))

  // Queues holding memory requests from PEs
  val reqQueues = Seq.fill(n_queue)(
    Module(new BankAccessQueue(p_width, addr_width, n_banks, reqQSize, respQSize))
  )
  // Bank selector connecting to each SRAM bank
  val bankSelectors =
    Seq.fill(n_banks)(Module(new TwoBankSelector(p_width, addr_width, n_queue)))

  for (i <- 0 until n_banks) {
    // Connect selectors to passthrough external IO
    bankSelectors(i).io.portOut_1 <> io.portOut_1(i)
    bankSelectors(i).io.portOut_2 <> io.portOut_2(i)
    // Connect queues and bank selectors
    for (j <- 0 until n_queue) {
      bankSelectors(i).io.inSeq(j) <> reqQueues(j).io.outSeq(i)
      bankSelectors(i).io.reqDone(j) <> reqQueues(j).io.reqDone(i)
    }
  }
  // Connect queues to passthrough external IO
  for (j <- 0 until n_queue) {
    io.inReq(j) <> reqQueues(j).io.inReq
    io.outData(j) <> reqQueues(j).io.outData
  }
  // Busy if not all request queues are empty
  io.busy := !reqQueues.map(_.io.empty).reduce(_ & _)

  def connectMems(mems: Seq[BRAMLikeMem]) {
    for (i <- 0 until n_banks) {
      // Connect the selector IO on the mem side
      mems(i).io.write_1 := io.portOut_1(i).write
      mems(i).io.addr_1 := io.portOut_1(i).addr
      mems(i).io.dataIn_1 := io.portOut_1(i).dataIn
      mems(i).io.enable_1 := io.portOut_1(i).valid
      io.portOut_1(i).dataOut := mems(i).io.dataOut_1
      mems(i).io.write_2 := io.portOut_2(i).write
      mems(i).io.addr_2 := io.portOut_2(i).addr
      mems(i).io.dataIn_2 := io.portOut_2(i).dataIn
      mems(i).io.enable_2 := io.portOut_2(i).valid
      io.portOut_2(i).dataOut := mems(i).io.dataOut_2
    }
  }
}
