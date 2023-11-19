package sygs.pe.dfe

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sygs.pe.dfe.submodules._
import sygs.Util._

import scala.collection.mutable.ListBuffer
import scala.util.Random

class FloatInverterTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "float inverter"
  it should "produce the right results (float)" in
    test(new FloatInverter(8, 24)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      c.io.out.ready.poke(true.B)

      for(n <- 0 until 100) {

        var in = 0.0f
        while(in == 0) in = Random.nextFloat()

        c.io.input.bits.bits.poke(floatToUInt(in))
        c.io.input.valid.poke(true.B)
        c.clock.step()
        c.io.input.valid.poke(false.B)

        val result = 1 / in

        var cycles = 1
        while(!c.io.out.valid.peekBoolean()) {

          c.clock.step()
          cycles += 1
        }

        println(cycles + " cycles")

        c.io.out.bits.bits.expect(floatToUInt(result))
        c.io.out.valid.expect(true.B)

        c.clock.step()
      }
    }
  it should "produce the right results (double)" in
    test(new FloatInverter(11, 53)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      c.io.out.ready.poke(true.B)

      for(n <- 0 until 100) {

        var in = 0.0
        while(in == 0) in = Random.nextDouble()

        c.io.input.bits.bits.poke(doubleToUInt(in))
        c.io.input.valid.poke(true.B)
        c.clock.step()
        c.io.input.valid.poke(false.B)

        val result = 1 / in

        var cycles = 1
        while(!c.io.out.valid.peekBoolean()) {

          c.clock.step()
          cycles += 1
        }

        println(cycles + " cycles")

        c.io.out.bits.bits.expect(doubleToUInt(result))
        c.io.out.valid.expect(true.B)

        c.clock.step()
      }
    }
  it should "produce the right results using pipeline (double)" in
    test(new FloatInverter(11, 53)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      c.io.out.ready.poke(true.B)

      val inputs = ListBuffer.fill(100)(Random.nextDouble())
      var currIn = 0
      var currRes = 0
      var cycles = 0

      while(currRes < inputs.size) {

        if(c.io.input.ready.peekBoolean() && currIn < inputs.size) {

          c.io.input.bits.bits.poke(doubleToUInt(inputs(currIn)))
          c.io.input.valid.poke(true.B)
          currIn += 1
        }

        if(c.io.out.valid.peekBoolean()) {

          println(cycles + " cycles")
          cycles = 0
          c.io.out.bits.bits.expect(doubleToUInt(1 / inputs(currRes)))
          currRes += 1
        }

        c.clock.step()
        cycles += 1
        c.io.input.valid.poke(false.B)
      }
    }
}