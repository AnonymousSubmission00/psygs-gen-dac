package spatial_templates

import chisel3._
import chisel3.experimental.IO
import chisel3.util._

class CtrlInterfaceIO extends Bundle {
  val ctrl_cmd = Flipped(Decoupled(Bits(14.W)))
  val idle = Output(Bool())
}

/** Extends the PE interface with the controller to include an interface to read
  * and write a BRAM-like memory with two read/write ports. Adds an interface
  * with the controller which enables to read and write data to the PE memory
  * from outside.
  *
  * @param p_width
  *   port width for the controller interface
  * @param entries
  *   number of entries for the memory
  */
class BRAMLikeMemInterfaceIO(val p_width: Int, val entries: Int)
    extends Bundle {
  val mem = Flipped(new BRAMLikeIO(p_width, log2Ceil(entries)))
  val ctrl_to_mem = new BRAMLikeIO(p_width, log2Ceil(entries))
  val ctrl_mem_active = Output(Bool())
}

/** PE able to communicate with a generic controller interface. A functional
  * extension of this class needs to give meaninful values to io.idle and
  * io.ctrl_cmd.ready
  *
  * @param id
  * @param mem_interfaces
  * @param pe_interfaces
  */
class ControlledPE(id: ElemId, io_bundle: CtrlInterfaceIO) extends PE(id) {
  val io = IO(io_bundle)
}

/** Blink PE able to communicate with a generic controller interface.
  *
  * @param id
  */
class BlinkPE(id: ElemId, io_bundle: CtrlInterfaceIO)
    extends ControlledPE(id, io_bundle) {
  val s_idle :: s_firing :: Nil = Enum(2)
  val state = RegInit(s_idle)

  state := s_idle
  io.idle := state === s_idle
  io.ctrl_cmd.ready := state === s_idle
  when(io.ctrl_cmd.ready && io.ctrl_cmd.valid) {
    state := s_firing
  }
}

/** Generic PE interfacing with a BRAM-like memory. A meaningful extension of
  * this class needs to give meaningful values to the pe_to_mem._ interface and
  * the ctrl_mem_enable signal
  *
  * @param id
  * @param mem_io_bundle
  * @param io_bundle
  */
class PEWithBRAMMem(
    id: ElemId,
    mem_io_bundle: BRAMLikeMemInterfaceIO,
    io_bundle: CtrlInterfaceIO
) extends ControlledPE(id, io_bundle) {
  val m_io = IO(mem_io_bundle)

  val addr_width = log2Ceil(mem_io_bundle.entries)
  // TODO: attatch this to something that makes sense
  val pe_to_mem = Wire(new BRAMLikeIO(mem_io_bundle.p_width, addr_width))

  // Temp bundle for outputs of Flipped(BramLikeIO)
  val to_mem_bundle = new Bundle {
    val enable_1 = Bool()
    val write_1 = Bool()
    val addr_1 = UInt(addr_width.W)
    val dataIn_1 = UInt(mem_io_bundle.p_width.W)
    val enable_2 = Bool()
    val write_2 = Bool()
    val addr_2 = UInt(addr_width.W)
    val dataIn_2 = UInt(mem_io_bundle.p_width.W)
  }

  // Enable for Ctrl/PE to mem mux (in & out)
  val ctrl_mem_enable = Wire(Bool())
  m_io.ctrl_mem_active := ctrl_mem_enable

  val pe_to_mux_out = Wire(to_mem_bundle)
  val ctrl_to_mux_out = Wire(to_mem_bundle)

  // Signals from control to mux towards memory
  ctrl_to_mux_out.enable_1 := m_io.ctrl_to_mem.enable_1
  ctrl_to_mux_out.write_1 := m_io.ctrl_to_mem.write_1
  ctrl_to_mux_out.addr_1 := m_io.ctrl_to_mem.addr_1
  ctrl_to_mux_out.dataIn_1 := m_io.ctrl_to_mem.dataIn_1
  ctrl_to_mux_out.enable_2 := m_io.ctrl_to_mem.enable_2
  ctrl_to_mux_out.write_2 := m_io.ctrl_to_mem.write_2
  ctrl_to_mux_out.addr_2 := m_io.ctrl_to_mem.addr_2
  ctrl_to_mux_out.dataIn_2 := m_io.ctrl_to_mem.dataIn_2

  // Signals from pe to mux towards memory
  pe_to_mux_out.enable_1 := pe_to_mem.enable_1
  pe_to_mux_out.write_1 := pe_to_mem.write_1
  pe_to_mux_out.addr_1 := pe_to_mem.addr_1
  pe_to_mux_out.dataIn_1 := pe_to_mem.dataIn_1
  pe_to_mux_out.enable_2 := pe_to_mem.enable_2
  pe_to_mux_out.write_2 := pe_to_mem.write_2
  pe_to_mux_out.addr_2 := pe_to_mem.addr_2
  pe_to_mux_out.dataIn_2 := pe_to_mem.dataIn_2

  // Select output signals from either the controller or the PE towards memory
  val mem_io_mux_out = Mux(ctrl_mem_enable, ctrl_to_mux_out, pe_to_mux_out)

  // Connect mux output to mem interface output
  m_io.mem.enable_1 := mem_io_mux_out.enable_1
  m_io.mem.write_1 := mem_io_mux_out.write_1
  m_io.mem.addr_1 := mem_io_mux_out.addr_1
  m_io.mem.dataIn_1 := mem_io_mux_out.dataIn_1
  m_io.mem.enable_2 := mem_io_mux_out.enable_2
  m_io.mem.write_2 := mem_io_mux_out.write_2
  m_io.mem.addr_2 := mem_io_mux_out.addr_2
  m_io.mem.dataIn_2 := mem_io_mux_out.dataIn_2

  // Connect IO to data out from mem
  m_io.ctrl_to_mem.dataOut_1 := m_io.mem.dataOut_1
  m_io.ctrl_to_mem.dataOut_2 := m_io.mem.dataOut_2
  pe_to_mem.dataOut_1 := m_io.mem.dataOut_1
  pe_to_mem.dataOut_2 := m_io.mem.dataOut_2
}

class CtrlOnlyRWPE(
    id: ElemId,
    mem_io_bundle: BRAMLikeMemInterfaceIO,
    io_bundle: CtrlInterfaceIO
) extends PEWithBRAMMem(id, mem_io_bundle, io_bundle) {

  // No PE logic exists here
  pe_to_mem.enable_1 := false.B
  pe_to_mem.write_1 := false.B
  pe_to_mem.addr_1 := 0.U
  pe_to_mem.dataIn_1 := 0.U
  pe_to_mem.dataOut_1 := 0.U
  pe_to_mem.enable_2 := false.B
  pe_to_mem.write_2 := false.B
  pe_to_mem.addr_2 := 0.U
  pe_to_mem.dataIn_2 := 0.U
  pe_to_mem.dataOut_2 := 0.U
  // Always enable the controller to write
  ctrl_mem_enable := true.B
  // Does not accept commands, since there is no logic, just passthrough to mem
  io.ctrl_cmd.ready := false.B
  io.idle := true.B
}

/*
/** Generic PE with a BRAM-like memory and a Lite interface interconnection
  * list. The number of interconnections is determined by the InterfaceGenerator
  * given as input
  *
  * @param id
  * @param mem_io_bundle
  * @param io_bundle
  * @param i_gen
  */
class PEWithBRAMWithLiteInterface(
    id: ElemId,
    mem_io_bundle: BRAMLikeMemInterfaceIO,
    io_bundle: CtrlInterfaceIO,
    i_gen: InterfaceGenerator
) extends PEWithBRAMMem(id, mem_io_bundle, io_bundle)
    with LitePEInterfaces {

  val n_lite_io =
    i_gen.getConnectionsIDs(id).length // Function of the ID created by i_gen
  override val lite_interfaces = Seq.fill(n_lite_io)(
    IO(
      new PEBidirectionalLiteInterface(
        mem_io_bundle.p_width,
        log2Ceil(mem_io_bundle.entries)
      )
    )
  )
}

class BlinkPELite(
    id: ElemId,
    mem_io_bundle: BRAMLikeMemInterfaceIO,
    io_bundle: CtrlInterfaceIO,
    i_gen: InterfaceGenerator
) extends PEWithBRAMWithLiteInterface(id, mem_io_bundle, io_bundle, i_gen) {

  val s_0 :: s_1 :: s_2 :: Nil = Enum(3)
  val state = RegInit(s_0)

  // Does not read inputs
  lite_interfaces.map { _.ch_in.ready := false.B }

  // Default outs
  lite_interfaces.map { _.ch_out.valid := false.B }
  lite_interfaces.map { _.ch_out.bits.data := 0.U }
  lite_interfaces.map { _.ch_out.bits.addr := 0.U }

  when(state === s_0) {
    lite_interfaces(0).ch_out.valid := true.B
    lite_interfaces(0).ch_out.bits.data := 1.U
    lite_interfaces(0).ch_out.bits.addr := 1.U
    state := s_1
  }
  when(state === s_1) {
    lite_interfaces(1).ch_out.valid := true.B
    lite_interfaces(1).ch_out.bits.data := 2.U
    lite_interfaces(1).ch_out.bits.addr := 2.U
    state := s_2
  }
  when(state === s_2) {
    lite_interfaces(2).ch_out.valid := true.B
    lite_interfaces(2).ch_out.bits.data := 3.U
    lite_interfaces(2).ch_out.bits.addr := 3.U
    state := s_0
  }

  // No PE logic exists here
  pe_to_mem.enable_1 := false.B
  pe_to_mem.write_1 := false.B
  pe_to_mem.addr_1 := 0.U
  pe_to_mem.dataIn_1 := 0.U
  pe_to_mem.dataOut_1 := 0.U
  pe_to_mem.enable_2 := false.B
  pe_to_mem.write_2 := false.B
  pe_to_mem.addr_2 := 0.U
  pe_to_mem.dataIn_2 := 0.U
  pe_to_mem.dataOut_2 := 0.U
  // Always enable the controller to write
  ctrl_mem_enable := true.B
  // Does not accept commands, since there is no logic, just passthrough to mem
  io.ctrl_cmd.ready := false.B
  io.idle := true.B
}*/
