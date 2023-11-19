package sygs.pe.dfe

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import spatial_templates._
import sygs.Util._

import scala.collection.mutable.ListBuffer
import scala.util.Random

class SyGSDFETest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "dataflow engine"
  it should "produce the right results" in
    test(new SyGSDFE(1, 1, 10, 10, 10, 11, 53, false, false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        c.io.out.ready.poke(true.B)

        for(n <- 0 until 10) {

          c.io.rowValues(0).ready.expect(true.B)
          c.io.variables(0).ready.expect(true.B)
          c.io.initSums(0).ready.expect(true.B)
          c.io.numbersOfNonZeros(0).ready.expect(true.B)
          c.io.diagonalValues(0).ready.expect(true.B)

          var sum1 = Random.nextDouble()
          var sum2 = 0d
          val numberOfNonZeros = Random.nextInt(31) + 2
          val diagonalValue = Random.nextDouble()
          val invertedValue = 1 / diagonalValue

          c.io.initSums(0).bits.bits.poke(doubleToUInt(sum1))
          c.io.numbersOfNonZeros(0).bits.poke(numberOfNonZeros.U)
          c.io.diagonalValues(0).bits.bits.poke(doubleToUInt(diagonalValue))

          c.io.initSums(0).valid.poke(true.B)
          c.io.numbersOfNonZeros(0).valid.poke(true.B)
          c.io.diagonalValues(0).valid.poke(true.B)

          var cycles = 0
          for(i <- 0 until numberOfNonZeros - 1) {

            val x = Random.nextDouble()
            val y = Random.nextDouble()

            val temp = sum2
            sum2 = sum1 - (x * y)
            sum1 = temp

            c.io.rowValues(0).bits.bits.poke(doubleToUInt(x))
            c.io.variables(0).bits.bits.poke(doubleToUInt(y))
            c.io.rowValues(0).valid.poke(true.B)
            c.io.variables(0).valid.poke(true.B)
            c.clock.step()
            cycles += 1

            if(i == 0) {

              c.io.numbersOfNonZeros(0).valid.poke(false)
              c.io.diagonalValues(0).valid.poke(false.B)
              c.io.initSums(0).valid.poke(false)
            }
          }
          c.io.rowValues(0).valid.poke(false.B)
          c.io.variables(0).valid.poke(false.B)

          c.clock.step()
          cycles += 1

          while(!c.io.out.valid.peek().litToBoolean) {

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
  it should "work under heavy workload" in
    test(new SyGSDFE(1, 4, 10, 10, 10, 11, 53, false, false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        c.io.out.ready.poke(true.B)

        val iterations = 10

        val sumArr1 = ListBuffer[Double]()
        val numberOfNonZerosArr = ListBuffer[Int]()
        val diagonalValueArr = ListBuffer[Double]()
        val rowValueArr = ListBuffer[ListBuffer[Double]]()
        val variableArr = ListBuffer[ListBuffer[Double]]()
        val resultArr = ListBuffer[Double]()

        for (n <- 0 until iterations) {

          var sum1 = Random.nextDouble()
          var sum2 = 0d
          sumArr1 += sum1
          numberOfNonZerosArr += Random.nextInt(31) + 2
          diagonalValueArr += Random.nextDouble()

          val rowValues = ListBuffer[Double]()
          val variables = ListBuffer[Double]()

          for (i <- 0 until numberOfNonZerosArr(n) - 1) {

            val x = Random.nextDouble()
            val y = Random.nextDouble()

            rowValues += x
            variables += y

            val temp = sum2
            sum2 = sum1 - (x * y)
            sum1 = temp
          }

          val invertedValue = 1 / diagonalValueArr(n)

          rowValueArr += rowValues
          variableArr += variables
          resultArr += (sum1 + sum2) * invertedValue
        }

        c.io.rowValues(0).ready.expect(true.B)
        c.io.variables(0).ready.expect(true.B)
        c.io.initSums(0).ready.expect(true.B)
        c.io.numbersOfNonZeros(0).ready.expect(true.B)
        c.io.diagonalValues(0).ready.expect(true.B)

        var nMultiplier = 0
        var iMultiplier = 0
        var nAccumulator = 0
        var nDivider = 0
        var nOut = 0

        c.io.out.ready.poke(true.B)

        var cycles = 0

        while(nOut < iterations) {

          //feed the multiplier
          if(c.io.rowValues(0).ready.peekBoolean() && c.io.variables(0).ready.peekBoolean() && nMultiplier < iterations) {

            println(s"multiplier ready at cycle $cycles")

            c.io.rowValues(0).bits.bits.poke(doubleToUInt(rowValueArr(nMultiplier)(iMultiplier)))
            c.io.variables(0).bits.bits.poke(doubleToUInt(variableArr(nMultiplier)(iMultiplier)))
            c.io.rowValues(0).valid.poke(true.B)
            c.io.variables(0).valid.poke(true.B)

            iMultiplier += 1

            if(iMultiplier == numberOfNonZerosArr(nMultiplier) - 1) {

              iMultiplier = 0
              nMultiplier += 1
            }
          } else {

            c.io.rowValues(0).valid.poke(false.B)
            c.io.variables(0).valid.poke(false.B)
          }

          //feed the accumulator
          if (c.io.initSums(0).ready.peekBoolean() && c.io.numbersOfNonZeros(0).ready.peekBoolean() && nAccumulator < iterations) {

            println(s"accumulator ready at cycle $cycles")
            println("number of cycles for next calculation:" + (numberOfNonZerosArr(nAccumulator) - 1))

            c.io.initSums(0).bits.bits.poke(doubleToUInt(sumArr1(nAccumulator)))
            c.io.numbersOfNonZeros(0).bits.poke(numberOfNonZerosArr(nAccumulator))
            c.io.initSums(0).valid.poke(true.B)
            c.io.numbersOfNonZeros(0).valid.poke(true.B)

            nAccumulator += 1
          } else {

            c.io.initSums(0).valid.poke(false.B)
            c.io.numbersOfNonZeros(0).valid.poke(false.B)
          }

          //feed the divider
          if(c.io.diagonalValues(0).ready.peekBoolean() && nDivider < iterations) {

            println(s"divider ready at cycle $cycles")

            c.io.diagonalValues(0).bits.bits.poke(doubleToUInt(diagonalValueArr(nDivider)))
            c.io.diagonalValues(0).valid.poke(true.B)

            nDivider += 1
          } else {

            c.io.diagonalValues(0).valid.poke(false.B)
          }

          c.clock.step()
          cycles += 1

          //collect results
          if(c.io.out.valid.peekBoolean()) {

            c.io.out.bits.bits.expect(doubleToUInt(resultArr(nOut)))

            val output = c.io.out.bits.bits.peekInt()
            println(s"\nComputation $nOut: ready in $cycles cycles")
            print("variables :")
            println(variableArr(nOut).mkString("ListBuffer(", ", ", ")"))
            print("rowValues :")
            println(rowValueArr(nOut).mkString("ListBuffer(", ", ", ")"))
            print("sum :")
            println(sumArr1(nOut))
            print("result :")
            println(resultArr(nOut))
            print("output :")
            println(bigIntToDouble(output))

            nOut += 1

            cycles = 0
          }
        }
      }
  it should "work with random latencies" in
    test(new SyGSDFE(1, 2, 10, 10, 10, 11, 53, false, false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        c.io.out.ready.poke(true.B)

        val iterations = 10


        val sumArr = ListBuffer[Double]()
        val numberOfNonZerosArr = ListBuffer[Int]()
        val diagonalValueArr = ListBuffer[Double]()
        val rowValueArr = ListBuffer[ListBuffer[Double]]()
        val variableArr = ListBuffer[ListBuffer[Double]]()
        val partialResultArr = ListBuffer[Double]()
        val resultArr = ListBuffer[Double]()

        for (n <- 0 until iterations) {

          var sum1 = Random.nextDouble()
          var sum2 = 0d
          sumArr += sum1
          numberOfNonZerosArr += Random.nextInt(31) + 2
          diagonalValueArr += Random.nextDouble()

          val rowValues = ListBuffer[Double]()
          val variables = ListBuffer[Double]()

          for (i <- 0 until numberOfNonZerosArr(n) - 1) {

            val x = Random.nextDouble()
            val y = Random.nextDouble()

            rowValues += x
            variables += y

            val temp = sum2
            sum2 = sum1 - (x * y)
            sum1 = temp
          }

          val invertedValue = 1 / diagonalValueArr(n)

          rowValueArr += rowValues
          variableArr += variables
          partialResultArr += sum1 + sum2
          resultArr += (sum1 + sum2) * invertedValue
        }

        c.io.rowValues(0).ready.expect(true.B)
        c.io.variables(0).ready.expect(true.B)
        c.io.initSums(0).ready.expect(true.B)
        c.io.numbersOfNonZeros(0).ready.expect(true.B)
        c.io.diagonalValues(0).ready.expect(true.B)

        var nMultiplier = 0
        var iMultiplier = 0
        var nAccumulator = 0
        var nDivider = 0
        var nOut = 0

        var cycles = 0

        while (nOut < iterations) {

          //c.io.out.ready.poke(Random.nextBoolean())

          //feed the multiplier
          if (c.io.rowValues(0).ready.peekBoolean() && c.io.variables(0).ready.peekBoolean() && nMultiplier < iterations && Random.nextBoolean()) {

            println(s"multiplier ready at cycle $cycles")

            c.io.rowValues(0).bits.bits.poke(doubleToUInt(rowValueArr(nMultiplier)(iMultiplier)))
            c.io.variables(0).bits.bits.poke(doubleToUInt(variableArr(nMultiplier)(iMultiplier)))
            c.io.rowValues(0).valid.poke(true.B)
            c.io.variables(0).valid.poke(true.B)

            iMultiplier += 1

            if (iMultiplier == numberOfNonZerosArr(nMultiplier) - 1) {

              iMultiplier = 0
              nMultiplier += 1
            }
          } else {

            c.io.rowValues(0).valid.poke(false.B)
            c.io.variables(0).valid.poke(false.B)
          }

          //feed the accumulator
          if (c.io.initSums(0).ready.peekBoolean() && c.io.numbersOfNonZeros(0).ready.peekBoolean() && nAccumulator < iterations && Random.nextBoolean()) {

            println(s"accumulator ready at cycle $cycles")
            println("number of cycles for next calculation:" + (numberOfNonZerosArr(nAccumulator) - 1))

            c.io.initSums(0).bits.bits.poke(doubleToUInt(sumArr(nAccumulator)))
            c.io.numbersOfNonZeros(0).bits.poke(numberOfNonZerosArr(nAccumulator))
            c.io.initSums(0).valid.poke(true.B)
            c.io.numbersOfNonZeros(0).valid.poke(true.B)

            nAccumulator += 1
          } else {

            c.io.initSums(0).valid.poke(false.B)
            c.io.numbersOfNonZeros(0).valid.poke(false.B)
          }

          //feed the divider
          if (c.io.diagonalValues(0).ready.peekBoolean() && nDivider < iterations && Random.nextBoolean()) {

            println(s"divider ready at cycle $cycles")

            c.io.diagonalValues(0).bits.bits.poke(doubleToUInt(diagonalValueArr(nDivider)))
            c.io.diagonalValues(0).valid.poke(true.B)

            nDivider += 1
          } else {

            c.io.diagonalValues(0).valid.poke(false.B)
          }

          c.clock.step()
          cycles += 1

          //collect results
          if (c.io.out.valid.peekBoolean()) {

            c.io.out.bits.bits.expect(doubleToUInt(resultArr(nOut)))

            val output = c.io.out.bits.bits.peekInt()
            println(s"\nComputation $nOut: ready in $cycles cycles")
            print("variables :")
            println(variableArr(nOut).mkString("ListBuffer(", ", ", ")"))
            print("rowValues :")
            println(rowValueArr(nOut).mkString("ListBuffer(", ", ", ")"))
            print("sum :")
            println(sumArr(nOut))
            print("partialResult :")
            println(partialResultArr(nOut))
            print("result :")
            println(resultArr(nOut))
            print("output :")
            println(bigIntToDouble(output))

            nOut += 1

            cycles = 0
          }
        }
      }
  it should "work with multiple accumulators" in
    test(new SyGSDFE(2, 1, 10, 10, 10, 11, 53, false, false))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        c.io.out.ready.poke(true.B)

        val iterations = 6
        val maxNumberOfNonZeros = 32

        val numberOfInputs = 5


        val sumArr = ListBuffer[Double]()
        val numberOfNonZerosArr = ListBuffer[Int]()
        val diagonalValueArr = ListBuffer[Double]()
        val rowValueArr = ListBuffer[ListBuffer[Double]]()
        val variableArr = ListBuffer[ListBuffer[Double]]()
        val resultArr = ListBuffer[Double]()

        val inputsSeq = Seq(rowValueArr, variableArr, sumArr, numberOfNonZerosArr, diagonalValueArr)

        for (n <- 0 until iterations) {

          var sum1 = Random.nextDouble()
          var sum2 = 0d
          sumArr += sum1
          numberOfNonZerosArr += Random.nextInt(maxNumberOfNonZeros - 1) + 1
          diagonalValueArr += Random.nextDouble()

          val rowValues = ListBuffer[Double]()
          val variables = ListBuffer[Double]()

          for (i <- 0 until numberOfNonZerosArr(n) - 1) {

            val x = Random.nextDouble()
            val y = Random.nextDouble()

            rowValues += x
            variables += y

            val temp = sum2
            sum2 = sum1 - (x * y)
            sum1 = temp
          }

          val invertedValue = 1 / diagonalValueArr(n)

          rowValueArr += rowValues
          variableArr += variables
          resultArr += (sum1 + sum2) * invertedValue
        }

        for (i <- 0 until c.numberOfAccumulators) {

          c.io.rowValues(i).ready.expect(true.B)
          c.io.variables(i).ready.expect(true.B)
          c.io.initSums(i).ready.expect(true.B)
          c.io.numbersOfNonZeros(i).ready.expect(true.B)
          c.io.diagonalValues(i).ready.expect(true.B)
        }

        //inputs
        val inputsAssignedComputations = ListBuffer.fill(numberOfInputs)(ListBuffer.fill(c.numberOfAccumulators)(ListBuffer[Int]()))

        //assign computations
        val computations = scala.util.Random.shuffle(ListBuffer.range(0, iterations))
        while (computations.nonEmpty) {

          for (i <- 0 until c.numberOfAccumulators) {

            if (computations.nonEmpty) {

              val comp = computations.remove(0)
              inputsAssignedComputations.foreach(input => input(i) += comp)
            }
          }
        }

        val inputsN = ListBuffer.fill(numberOfInputs)(ListBuffer.fill(c.numberOfAccumulators)(0))
        for (input <- 0 until numberOfInputs) {

          for (i <- 0 until c.numberOfAccumulators) {

            if (inputsAssignedComputations(input)(i).nonEmpty)

              inputsN(input)(i) = inputsAssignedComputations(input)(i).remove(0)
          }
        }

        val inputsI = ListBuffer.fill(2)(ListBuffer.fill(c.numberOfAccumulators)(0))

        val inputsPESeq = Seq(c.io.rowValues, c.io.variables, c.io.initSums, c.io.numbersOfNonZeros, c.io.diagonalValues)

        var cycle = 0
        val maxcycles = math.max(2 + 26 * iterations, 3 + maxNumberOfNonZeros * math.ceil(iterations / c.numberOfAccumulators))

        while (resultArr.nonEmpty && cycle <= maxcycles) {

          //feed the multipliers
          for (unit <- 0 until c.numberOfAccumulators) {

            for (input <- 0 until numberOfInputs) {

              if (inputsPESeq(input)(unit).ready.peekBoolean() && inputsN(input)(unit) < iterations) {

                val n = inputsN(input)(unit)

                if (input < inputsI.size) {

                  val i = inputsI(input)(unit)

                  if (i == numberOfNonZerosArr(n) - 2) {

                    inputsI(input)(unit) = 0
                    inputsN(input)(unit) = if (inputsAssignedComputations(input)(unit).nonEmpty) inputsAssignedComputations(input)(unit).remove(0) else iterations
                  } else {

                    inputsI(input)(unit) += 1
                  }

                  val value = inputsSeq(input)(n).asInstanceOf[ListBuffer[Double]](i)
                  inputsPESeq(input)(unit).bits.asInstanceOf[Float].bits.poke(doubleToUInt(value))
                } else if (input == 3) {

                  inputsN(input)(unit) = if (inputsAssignedComputations(input)(unit).nonEmpty) inputsAssignedComputations(input)(unit).remove(0) else iterations

                  val value = inputsSeq(input)(n).asInstanceOf[Int]
                  inputsPESeq(input)(unit).asInstanceOf[DecoupledIO[UInt]].bits.poke(value)
                } else {

                  inputsN(input)(unit) = if (inputsAssignedComputations(input)(unit).nonEmpty) inputsAssignedComputations(input)(unit).remove(0) else iterations

                  val value = inputsSeq(input)(n).asInstanceOf[Double]
                  inputsPESeq(input)(unit).bits.asInstanceOf[Float].bits.poke(doubleToUInt(value))
                }

                inputsPESeq(input)(unit).valid.poke(true.B)

              } else {

                inputsPESeq(input)(unit).valid.poke(false.B)
              }
            }
          }

          c.clock.step()
          cycle += 1
          println(s"cycle $cycle")

          //collect results
          if (c.io.out.valid.peekBoolean()) {

            val out = bigIntToDouble(c.io.out.bits.bits.peekInt())
            println(out)

            assert(resultArr.contains(out))
            resultArr.remove(resultArr.indexOf(out))
          }
        }

        assert(resultArr.isEmpty)
      }
}
