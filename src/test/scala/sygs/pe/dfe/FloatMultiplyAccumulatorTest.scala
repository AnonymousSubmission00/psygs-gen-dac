package sygs
package pe.dfe

import chisel3._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sygs.pe.dfe.submodules.FloatMultiplyAccumulator
import Util._

import scala.util.Random

class FloatMultiplyAccumulatorTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "float multiply accumulator"
  it should "produce the right results" in
    test(new FloatMultiplyAccumulator(10, 10, 10, 11, 53, false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        c.io.out.ready.poke(true.B)

        for (n <- 0 until 10) {

          c.io.rowValue.ready.expect(true.B)
          c.io.variable.ready.expect(true.B)
          c.io.initSum.ready.expect(true.B)
          c.io.numbersOfNonZero.ready.expect(true.B)
          c.io.diagonalValue.ready.expect(true.B)
          c.io.invertedValue.ready.expect(true.B)

          var sum1 = Random.nextDouble()
          var sum2 = 0d
          val numberOfNonZeros = Random.nextInt(31) + 2
          val diagonalValue = Random.nextDouble()
          val invertedValue = 1d/diagonalValue

          c.io.initSum.bits.bits.poke(doubleToUInt(sum1))
          c.io.numbersOfNonZero.bits.poke(numberOfNonZeros.U)
          c.io.diagonalValue.bits.bits.poke(doubleToUInt(diagonalValue))
          c.io.invertedValue.bits.bits.poke(doubleToUInt(invertedValue))

          c.io.initSum.valid.poke(true.B)
          c.io.numbersOfNonZero.valid.poke(true.B)
          c.io.diagonalValue.valid.poke(true.B)
          c.io.invertedValue.valid.poke(true.B)

          var cycles = 0
          for (i <- 0 until numberOfNonZeros - 1) {

            val x = Random.nextDouble()
            val y = Random.nextDouble()

            val temp = sum2
            sum2 = sum1 - (x * y)
            sum1 = temp

            c.io.rowValue.bits.bits.poke(doubleToUInt(x))
            c.io.variable.bits.bits.poke(doubleToUInt(y))
            c.io.rowValue.valid.poke(true.B)
            c.io.variable.valid.poke(true.B)
            c.clock.step()
            cycles += 1

            if (i == 0) {
              c.io.invertedValue.valid.poke(false.B)
              c.io.diagonalValue.valid.poke(false.B)
              c.io.initSum.valid.poke(false.B)
              c.io.numbersOfNonZero.valid.poke(false.B)
            }
          }
          c.io.rowValue.valid.poke(false.B)
          c.io.variable.valid.poke(false.B)

          c.io.diagonalValueOut.bits.bits.expect(doubleToUInt(diagonalValue))
          c.io.diagonalValueOut.valid.expect(true.B)
          c.io.diagonalValueOut.ready.poke(true.B)

          c.clock.step()
          cycles += 1

          c.io.diagonalValueOut.valid.expect(false.B)
          c.io.diagonalValueOut.ready.poke(false.B)

          while (!c.io.out.valid.peek().litToBoolean) {

            c.clock.step()
            cycles += 1
          }
          println(s"Cycles: $cycles")

          val result = (sum1 + sum2) * invertedValue

          c.io.out.bits.bits.expect(doubleToUInt(result))
          c.io.out.valid.expect(true.B)
          c.io.out.ready.poke(true.B)
        }
      }
}
