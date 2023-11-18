package sygs
package pe.dfe

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sygs.pe.dfe.submodules.FloatMultiplier
import Util._

import scala.util.Random

class FloatMultiplierTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "float multiplier"
  it should "produce the right results" in
    test(new FloatMultiplier(8, 24)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      c.io.out.ready.poke(true.B)

      for(n <- 0 until 10) {

        val in1 = (Random.nextFloat() + 0.1f) * Random.nextInt()
        val in2 = (Random.nextFloat() + 0.1f) * Random.nextInt()

        c.io.inputs.bits.in1.bits.poke(floatToUInt(in1))
        c.io.inputs.bits.in2.bits.poke(floatToUInt(in2))

        c.io.inputs.valid.poke(true.B)

        val result = in1 * in2

        c.io.out.bits.bits.expect(floatToUInt(result))
        c.io.out.valid.expect(true.B)

        c.clock.step()
      }
    }

  it should "not be ready if out not ready" in
    test(new FloatMultiplier(8, 24)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      c.io.out.ready.poke(false.B)

      val in1 = (Random.nextFloat() + 0.1f) * Random.nextInt()
      val in2 = (Random.nextFloat() + 0.1f) * Random.nextInt()

      c.io.inputs.bits.in1.bits.poke(floatToUInt(in1))
      c.io.inputs.bits.in2.bits.poke(floatToUInt(in2))

      c.io.inputs.valid.poke(true.B)

      c.io.inputs.ready.expect(false.B)
      c.io.out.valid.expect(false.B)

      c.clock.step()

      c.io.out.ready.poke(true.B)

      val result = in1 * in2

      c.io.out.bits.bits.expect(floatToUInt(result))
      c.io.inputs.ready.expect(true.B)
      c.io.out.valid.expect(true.B)
    }
}
