package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import spatial_templates._

/**
  * This modules implements a multiple input multiple output (MIMO) inverter.
  */
class FloatInverterMIMO(val numberOfAccumulators: Int, dividerQueueSize: Int, exp: Int, sign: Int) extends Module {

  assert(numberOfAccumulators > 0)
  assert(dividerQueueSize > 0)

  val io = IO(new Bundle() {

    val diagonalValue = Vec(numberOfAccumulators, Flipped(Decoupled(Float(exp, sign))))

    val out = Vec(numberOfAccumulators, Decoupled(Float(exp, sign)))
  })

  //instantiate module
  private val inverter = Module(new FloatInverter(exp, sign))

  //register
  private val producerIndex = Wire(Decoupled(UInt()))
  private val producerIndexQueue = Queue(producerIndex, 3)

  //divider arbiter
  private val arbiter = Module(new Arbiter(Float(exp, sign), numberOfAccumulators))

  //setting arbiter
  arbiter.io.in.zipWithIndex.foreach { case (arbiterIn, i) =>

    arbiterIn.bits := io.diagonalValue(i).bits
    arbiterIn.valid := io.diagonalValue(i).valid && io.out(i).ready
    io.diagonalValue(i).ready := arbiterIn.ready
  }
  arbiter.io.out.ready := inverter.io.input.ready

  //producer index
  producerIndex.bits := arbiter.io.chosen
  producerIndex.valid := inverter.io.input.ready && arbiter.io.in(arbiter.io.chosen).valid

  //setting inverter
  inverter.io.input.bits := arbiter.io.out.bits
  inverter.io.input.valid := arbiter.io.out.valid

  //returning invertedValue
  inverter.io.out.ready := false.B
  producerIndexQueue.ready := false.B
  io.out.zipWithIndex.foreach { case (o, i) =>

    when(i.U === producerIndexQueue.bits) {

      inverter.io.out.ready := o.ready
      producerIndexQueue.ready := o.ready
    }
    o.bits := inverter.io.out.bits
    o.valid := inverter.io.out.valid && i.U === producerIndexQueue.bits
  }
}
