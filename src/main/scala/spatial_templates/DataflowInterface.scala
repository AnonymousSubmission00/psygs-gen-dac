package spatial_templates

import chisel3._
import chisel3.util._

class DataflowMemQueueIO(
    val p_width: Int,
    val addr_width: Int,
    val n_queue: Int,
    val n_read_queues: Int
) extends Bundle {
  // Interface to the mem request queues
  val outgoingReq =
    Vec(n_queue, Decoupled(new MemRequestIO(p_width, addr_width)))
  // Outcome of a memory read request
  val inReadData = Vec(n_read_queues, Flipped(Valid(UInt(p_width.W))))  

  val readQueuesEnqReady = Vec(n_read_queues, Output(Bool()))
}

/** Generic interface for a dataflow PE
  */
class DataflowPEIO(val p_width: Int, val addr_width: Int, n_queue: Int, n_read_queues: Int) extends Bundle {
  val memQueuesIO = new DataflowMemQueueIO(p_width, addr_width, n_queue, n_read_queues)
}

//TODO integrate with spatial templates
/** Dataflow PE. This specialized PE extends a ControlledPE to have a
  * queue-based decoupled interface.
  *
  * @param id
  * @param io_bundle
  *   .inReq is a Vec of decoupled MemRequestIO interfacing with the memory
  *   queues .inReadData is a ValidIO UInt of [data width] used for the
  *   responses to memory read requests
  * @param rQueueDepth
  *   can be 0 (no read queues)
  */
class DataflowPE(
    id: ElemId,
    io_bundle: CtrlInterfaceIO,
    p_width : Int,
    addr_width: Int,
    n_queue : Int,
    readQueues: Int,
    rQueueDepth: Int
) extends ControlledPE(id, io_bundle) {

  assert(rQueueDepth >= 0)

  val df_io = IO(new DataflowPEIO(p_width, addr_width, n_queue, readQueues))

  // Drive to 0 the valid bit for the write-only queues, by default at the start
  //var writeOnlyQueuesCount = n_queue - readQueues
  //for ((elem, i) <- df_io.memQueuesIO.inReadData.zipWithIndex) {
  //  while (writeOnlyQueuesCount > 0) {
  //    // !! this must be done by whoever instances the PE !! 
  //    //elem.valid := false.B
  //    //elem.bits := 0.U
  //    writeOnlyQueuesCount -= 1
  //  }
  //}
  val memReadQueuesEnq = Seq.fill(readQueues)(Wire(Valid(UInt())))
  val memReadQueues = Seq.fill(if (rQueueDepth > 0) readQueues else 0)(Module(new Queue(UInt(), rQueueDepth)))
  val memReadQueuesDeq = Seq.fill(readQueues)(Wire(Decoupled(UInt())))
  if (readQueues > 0) {

    for ((enq, i) <- memReadQueuesEnq.zipWithIndex) {

      enq.bits := df_io.memQueuesIO.inReadData(i).bits
      enq.valid := df_io.memQueuesIO.inReadData(i).valid
      if (rQueueDepth > 0)
        df_io.memQueuesIO.readQueuesEnqReady(i) := memReadQueues(i).io.count < (memReadQueues(i).io.entries.U - 1.U)
      else
        df_io.memQueuesIO.readQueuesEnqReady(i) := memReadQueuesDeq(i).ready
    }

    if (rQueueDepth > 0)
      for ((queue, i) <- memReadQueues.zipWithIndex) {

        queue.io.enq.bits := memReadQueuesEnq(i).bits
        queue.io.enq.valid := memReadQueuesEnq(i).valid
        memReadQueuesDeq(i) <> queue.io.deq
      }
    else
      for ((enq, deq) <- memReadQueuesEnq.zip(memReadQueuesDeq)) {

        deq.bits := enq.bits
        deq.valid := enq.valid
      }
  } else {
    def connectToReadQueue(queue_idx: Int, data: Bits, valid: Bool, ready: Bool) {}
  }

  def connectToOutgoingRequest(queue_idx: Int, write: Bool, addr: Bits, dataIn: Bits, ready: Bool, valid: Bool) {
    df_io.memQueuesIO.outgoingReq(queue_idx).bits.write := write
    df_io.memQueuesIO.outgoingReq(queue_idx).bits.addr := addr
    df_io.memQueuesIO.outgoingReq(queue_idx).bits.dataIn := dataIn
    df_io.memQueuesIO.outgoingReq(queue_idx).valid := valid 
    ready := df_io.memQueuesIO.outgoingReq(queue_idx).ready
  }
  def connectToReadQueue(queue_idx: Int, data: Bits, valid: Bool, ready: Bool) {
    data := memReadQueuesDeq(queue_idx).bits
    valid := memReadQueuesDeq(queue_idx).valid
    memReadQueuesDeq(queue_idx).ready := ready
  }
  //TODO integrate with spatial templates
  def connectToReadQueue(queue_idx: Int, data: DecoupledIO[Bits]) {
    data <> memReadQueuesDeq(queue_idx)
  }

  def connectElement(lhs: Data, rhs: Data) { lhs := rhs }
}

/*
/**
 * Provides the interface to instantiate a DFE
 */
trait WithDFE {
  val pe_dfe: AbstractDataflowEngine

  def instantiateDataflowEngine(dfe: AbstractDataflowEngine) {
    val pe_dfe = Module(dfe)
  }
}

/**
 * Provides an interface to connect a WithPassthroughDFE to an
 * external passthrough IO bundle
 */
trait WithPassthroughDFE extends WithDFE {
  val dfe_ctrl_io: DFEPassthroughIO
  val pe_dfe: PassthroughDataflowEngine

  def connectDFEPassthrough() {
    dfe_ctrl_io <> pe_dfe.ctrl_io
  }
}


/**
 * Provides the interface to instantiate an address generator
 */
trait WithAddrGen {
  val pe_addr_gen: AbstractAddressGenerator

  def instantiateAddresGenerator(addr_gen : AbstractAddressGenerator) {
    val pe_addr_gen = Module(addr_gen)
  }
}

/**
 * Provides an interface to connect a PassthroughAddressGenerator to an
 * external passthrough IO bundle
 */
trait WithPassthroughAddrGen extends WithAddrGen {
  val pe_addr_gen: AddrGenWithPassthrough
  val addr_gen_ctrl_io: AddressGeneratorPassthroughIO
  def connectAddrGenPassthrough() {
    addr_gen_ctrl_io <> pe_addr_gen.pe_ctrl_io
  }
}

/**
  * Provides an interface to connect and address generator to memory elements
  */
trait WithAddrGenToMem  extends WithAddrGen {
  val pe_addr_gen: AddrGenWithME
  def connectToMEInterface()
}

/**
  * Provides an interface to connect this address generator to a dataflow engine
  */
trait WithAddrGenToDFE extends WithAddrGen {
  val pe_addr_gen: AddrGenWithDFE
  def connectAddrGenToDFE()
}*/
