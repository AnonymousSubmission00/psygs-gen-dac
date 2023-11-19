package sygs.pe.dfe

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sygs.pe.dfe.submodules._
import sygs.Util._

import scala.util.Random

class FloatAccumulatorTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "float accumulator"
  it should "produce the right results" in
    test(new FloatAccumulator(11, 53, false)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      var meanErr = 0d
      var maxErr = 0d

      val iterations = 100

      for(n <- 0 until iterations) {
        c.io.out.ready.poke(true.B)

        var sum1 = Random.nextDouble()
        var sum2 = 0d
        val cycles = Random.nextInt(20) + 1

        c.io.initInputs.bits.init.bits.poke(doubleToUInt(sum1))
        c.io.initInputs.bits.cycles.poke(cycles.U)

        c.io.initInputs.valid.poke(true.B)

        c.clock.step()

        c.io.initInputs.valid.poke(false)

        c.io.out.bits.bits.expect(doubleToUInt(sum1))
        c.io.out.valid.expect(false.B)

        for (n <- 0 until cycles) {

          val in1 = Random.nextDouble()

          c.io.in1.bits.bits.poke(doubleToUInt(in1))
          c.io.in1.valid.poke(true.B)

          val temp = sum2
          sum2 = sum1 - in1
          sum1 = temp

          c.clock.step()
        }

        val sum = sum1 + sum2

        c.io.in1.valid.poke(false)

        c.clock.step()

        c.io.out.valid.expect(true.B)
        c.io.out.bits.bits.expect(doubleToUInt(sum))

        val result = bigIntToDouble(c.io.out.bits.bits.peekInt())
        val error = (result - sum)/sum

        meanErr += error/sum
        if (error > maxErr) maxErr = error

        println("sum: " + sum)
        println("result: " + result)
        println("error: " + error + "\n")
      }

      meanErr = meanErr/iterations

      println("\nFinish!\n")
      println("mean : " + meanErr)
      println("max : " + maxErr)
    }
}
