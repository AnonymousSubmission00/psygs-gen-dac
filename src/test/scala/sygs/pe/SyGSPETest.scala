package sygs
package pe

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.WriteVcdAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import spatial_templates._
import Util._

import scala.collection.mutable.ListBuffer
import scala.util.Random

class SyGSPETestModule(val numberOfPEs: Int,
                       inverterPerPE: Int,
                       val accumulatorPerPE: Int,
                       val variablesMemBanks: Int,
                       accumulatorMemAddressWidth: Int,
                       variablesMemAddressWidth: Int,
                       exp: Int,
                       sign: Int) extends Module {

  val io = IO(new Bundle() {

    val cmd = Input(Valid(UInt(1.W)))

    val accumulatorBramIOs = Vec(numberOfPEs * accumulatorPerPE, new BRAMLikeIO(exp + sign, accumulatorMemAddressWidth))
    val variableBramIOs = Vec(variablesMemBanks, new BRAMLikeIO(exp + sign, variablesMemAddressWidth))

    val computing = Output(Bool())
    val busy = Output(Bool())
  })

  private val pes = Seq.fill(numberOfPEs)(Module(
    new SyGSPE(accumulatorPerPE, inverterPerPE, variablesMemBanks, accumulatorMemAddressWidth, variablesMemAddressWidth,
      10, 10, false, 10, 2, 8, exp, sign, 3)))

  private val canProceed = Reg(Bool())
  when(!pes.map(pe => pe.coordination_io.computing).reduce(_ || _)) {

    canProceed := true.B
  }.elsewhen(pes.map(pe => pe.coordination_io.computing).reduce(_ && _)) {

    canProceed := false.B
  }.otherwise {

    canProceed := canProceed
  }
  pes.foreach { pe =>

    pe.io.ctrl_cmd.bits := io.cmd.bits
    pe.io.ctrl_cmd.valid := io.cmd.valid
    pe.coordination_io.canProceed := canProceed
  }

  private val busyReg = RegInit(Bool(), false.B)
  when(io.cmd.valid) { busyReg := true.B }
    .elsewhen(pes.map(pe => pe.io.idle).reduce(_ && _)) { busyReg := false.B }
    .otherwise { busyReg := busyReg }

  private val brams = Seq.fill(variablesMemBanks)(Module(
    new BRAMLikeMem1(new ElemId(1, 0, 0, 0), exp + sign, variablesMemAddressWidth)))

  private val variablesMemInterface = Module(
    new NtoMMemInterface(exp + sign, variablesMemAddressWidth, variablesMemBanks,
      2 * accumulatorPerPE * numberOfPEs, 5, 0))

  io.variableBramIOs.foreach { io =>

    io.dataOut_1 := 0.U
    io.dataOut_2 := 0.U
  }
  variablesMemInterface.connectMems(brams)
  when(io.cmd.valid || busyReg) { variablesMemInterface.connectMems(brams) }
    .otherwise { brams.zip(io.variableBramIOs).foreach { case (bram, io) => bram.io <> io } }

  private val accumulatorReqs = 9
  for(i <- 0 until numberOfPEs) {

    for(a <- 0 until accumulatorPerPE) {

      val bram = Module(
        new BRAMLikeMem1(new ElemId(1, 0, 0, 0), exp + sign, accumulatorMemAddressWidth))

      val accumulatorMemInterface = Module(
        new NtoMMemInterface(exp + sign, accumulatorMemAddressWidth, 1, accumulatorReqs, 5, 0))

      pes(i).connectToAccumulatorMemInterface(accumulatorMemInterface, a)
      pes(i).connectToVariablesMemInterface(variablesMemInterface, i, a)

      io.accumulatorBramIOs(i * accumulatorPerPE + a).dataOut_1 := 0.U
      io.accumulatorBramIOs(i * accumulatorPerPE + a).dataOut_2 := 0.U
      accumulatorMemInterface.connectMems(Seq(bram))
      when(io.cmd.valid || busyReg) { accumulatorMemInterface.connectMems(Seq(bram)) }
        .otherwise { bram.io <> io.accumulatorBramIOs(i * accumulatorPerPE + a) }
    }
  }

  io.busy := busyReg
  io.computing := pes.map(pe => pe.coordination_io.computing).reduce(_ || _)
}

class SyGSPETest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "processing element"
  it should "produce the right results (forward sweep)" in
    test(new SyGSPETestModule(2, 2, 1, 4, 7, 4, 11, 53))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        test(c, backwards = false, 2, 4, 4)
      }
  it should "produce the right results (backward sweep)" in
    test(new SyGSPETestModule(2, 2, 1, 4, 7, 4, 11, 53))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        test(c, backwards = true, 2, 4, 4)
      }
  it should "not get stuck under stress (backward sweep)" in
    test(new SyGSPETestModule(2, 2, 1, 4, 7, 4, 11, 53))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        stressTestWithoutCheck(c, backwards = true, 2, 4, 4)
      }
  it should "not get stuck" in
    test(new SyGSPETestModule(2, 2, 1, 4, 7, 4, 11, 53))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>

        test(c, backwards = true, 2, 0, 0)
      }

  def stressTestWithoutCheck(c: SyGSPETestModule, backwards: Boolean, colors: Int, maxRowsPerColor: Int, maxNumberOfNonZerosPerRow: Int): Unit = {

    c.clock.setTimeout(300)

    if(backwards) c.io.cmd.bits.poke(1.U)

    val numberOfAccumulators = c.numberOfPEs * c.accumulatorPerPE

    val matrixDim = maxRowsPerColor * maxNumberOfNonZerosPerRow

    val accMemBanks = ListBuffer.fill(numberOfAccumulators)(ListBuffer[UInt]())

    for(a <- 0 until numberOfAccumulators) {

      accMemBanks(a) += (0.U, 0.U, colors.U)

      var totRows = 0
      for(c <- 0 until colors) {

        val rows = Random.nextInt(maxRowsPerColor)
        accMemBanks(a) += rows.U

        totRows += rows
      }

      accMemBanks(a)(1) = totRows.U

      var totNZ = 0
      for(c <- 3 until colors + 3) {
        for(r <- 0 until accMemBanks(a)(c).litValue.intValue) {
          val nz = 1 + Random.nextInt(maxNumberOfNonZerosPerRow - 1)
          accMemBanks(a) += nz.U
          accMemBanks(a) += a.U
          accMemBanks(a) += Random.nextInt(Int.MaxValue).U
          totNZ += nz
        }
      }

      accMemBanks(a)(0) = totNZ.U

      for(nz <- 0 until totNZ)
        accMemBanks(a) += doubleToUInt(Random.nextDouble())

      for(nz <- 0 until totNZ)
        accMemBanks(a) += 0.U

      println("Accumulator " + a + ": " + accMemBanks(a).map(_.litValue.intValue))

    }

    val variables = ListBuffer.fill(matrixDim)(Random.nextDouble())

    writeVariablesOnMem(variables, c)

    println("Writing Data on accumulators memory")

    for(i <- 0 until accMemBanks.map(_.size).max by 2) {
      for(a <- 0 until numberOfAccumulators) {

        if(i < accMemBanks(a).size) {
          c.io.accumulatorBramIOs(a).write_1.poke(true)
          c.io.accumulatorBramIOs(a).addr_1.poke(i)
          c.io.accumulatorBramIOs(a).dataIn_1.poke(accMemBanks(a)(i))
        } else {
          c.io.accumulatorBramIOs(a).write_1.poke(false)
        }

        if(i+1 < accMemBanks(a).size) {
          c.io.accumulatorBramIOs(a).write_2.poke(true)
          c.io.accumulatorBramIOs(a).addr_2.poke(i + 1)
          c.io.accumulatorBramIOs(a).dataIn_2.poke(accMemBanks(a)(i + 1))
        } else {
          c.io.accumulatorBramIOs(a).write_2.poke(false)
        }
      }

      c.clock.step()
    }

    for(a <- 0 until numberOfAccumulators) {
      c.io.accumulatorBramIOs(a).write_1.poke(false)
      c.io.accumulatorBramIOs(a).write_2.poke(false)
    }

    c.clock.step()

    println("Starting execution")

    c.io.cmd.valid.poke(true)
    c.clock.step()
    c.io.cmd.valid.poke(false)

    var cycles = 0
    while(c.io.busy.peekBoolean()) {
      println(cycles)
      c.clock.step()
      cycles += 1
    }

    //to ensure all writes have been done
    c.clock.step(10)

    //verifyResults(resultVariables, c)

    println("\nSuccess!")
  }

  def test(c: SyGSPETestModule, backwards: Boolean, colors: Int, rowsPerColor: Int, numberOfNonZerosPerRow: Int): Unit = {

    c.clock.setTimeout(1000)

    if (backwards) c.io.cmd.bits.poke(1.U)

    val numberOfAccumulators = c.numberOfPEs * c.accumulatorPerPE

    val rowsPerColorPerAccumulator = rowsPerColor / numberOfAccumulators
    val matrixDim = rowsPerColor * numberOfNonZerosPerRow

    val totalNonZerosPerAccumulator = numberOfNonZerosPerRow * rowsPerColorPerAccumulator * colors
    val totalRowsPerAccumulator = rowsPerColorPerAccumulator * colors

    val initSums = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(Random.nextDouble()))
    val numberOfNonZerosArray = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(numberOfNonZerosPerRow))
    val diagonalIndices = ListBuffer.fill(colors)(ListBuffer[Int]())

    for (color <- 0 until colors)
      for (r <- 0 until rowsPerColor)
        diagonalIndices(color) += Random.nextInt(numberOfNonZerosArray(color)(r))

    val diagonalValues = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(0d))
    val diagonalColumnIndices = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(0))
    val rowValuesArray = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(ListBuffer[Double]()))
    val columnIndicesArray = ListBuffer.fill(colors)(ListBuffer.fill(rowsPerColor)(ListBuffer[Int]()))
    val variables = ListBuffer.fill(matrixDim)(Random.nextDouble())
    val resultVariables = variables.clone()

    writeVariablesOnMem(variables, c)

    for (a <- 0 until numberOfAccumulators) {

      c.io.accumulatorBramIOs(a).write_1.poke(true)
      c.io.accumulatorBramIOs(a).addr_1.poke(0)
      c.io.accumulatorBramIOs(a).dataIn_1.poke(totalNonZerosPerAccumulator)

      c.io.accumulatorBramIOs(a).write_2.poke(true)
      c.io.accumulatorBramIOs(a).addr_2.poke(1)
      c.io.accumulatorBramIOs(a).dataIn_2.poke(totalRowsPerAccumulator)
    }

    c.clock.step()

    for (a <- 0 until numberOfAccumulators) {

      c.io.accumulatorBramIOs(a).write_1.poke(true)
      c.io.accumulatorBramIOs(a).addr_1.poke(2)
      c.io.accumulatorBramIOs(a).dataIn_1.poke(colors)

      c.io.accumulatorBramIOs(a).write_2.poke(false)
    }

    c.clock.step()

    for (color <- 0 until colors) {

      for (a <- 0 until numberOfAccumulators) {

        c.io.accumulatorBramIOs(a).write_1.poke(true)
        c.io.accumulatorBramIOs(a).addr_1.poke(3 + color)
        c.io.accumulatorBramIOs(a).dataIn_1.poke(rowsPerColorPerAccumulator)
      }

      c.clock.step()
    }

    for (color <- 0 until colors) {

      for (r <- 0 until rowsPerColorPerAccumulator) {

        val rowIndices = ListBuffer.fill(numberOfAccumulators)(0)
        val offset = 3 + colors + 3 * (color * rowsPerColorPerAccumulator + r)

        for (a <- 0 until numberOfAccumulators) {

          rowIndices(a) = r + a * rowsPerColorPerAccumulator

          c.io.accumulatorBramIOs(a).write_1.poke(true)
          c.io.accumulatorBramIOs(a).addr_1.poke(offset)
          c.io.accumulatorBramIOs(a).dataIn_1.poke(numberOfNonZerosArray(color)(rowIndices(a)))
          c.io.accumulatorBramIOs(a).write_2.poke(true)
          c.io.accumulatorBramIOs(a).addr_2.poke(offset + 1)
          c.io.accumulatorBramIOs(a).dataIn_2.poke(diagonalIndices(color)(rowIndices(a)))
        }

        c.clock.step()

        for (a <- 0 until numberOfAccumulators) {

          c.io.accumulatorBramIOs(a).addr_1.poke(offset + 2)
          c.io.accumulatorBramIOs(a).dataIn_1.poke(doubleToUInt(initSums(color)(rowIndices(a))))

          c.io.accumulatorBramIOs(a).write_2.poke(false)
        }

        c.clock.step()
      }
    }

    for (color <- 0 until colors) {

      for (r <- 0 until rowsPerColorPerAccumulator) {

        val rowIndices = ListBuffer.fill(numberOfAccumulators)(0)
        val offset = 3 + colors + 3 * totalRowsPerAccumulator +
          (rowsPerColorPerAccumulator * color + r) * numberOfNonZerosPerRow

        for (i <- 0 until numberOfNonZerosPerRow by 2) {

          for (a <- 0 until numberOfAccumulators) {

            rowIndices(a) = r + a * rowsPerColorPerAccumulator

            val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
            val diagonalIndex = diagonalIndices(color)(rowIndices(a))
            val rowValues = rowValuesArray(color)(rowIndices(a))

            val rowValue1 = Random.nextDouble()
            val rowValue2 = Random.nextDouble()

            if (i == diagonalIndex) diagonalValues(color)(rowIndices(a)) = rowValue1
            else if (i < numberOfNonZeros) rowValues += rowValue1

            if (i + 1 == diagonalIndex) diagonalValues(color)(rowIndices(a)) = rowValue2
            else if (i + 1 < numberOfNonZeros) rowValues += rowValue2

            c.io.accumulatorBramIOs(a).addr_1.poke(offset + i)
            c.io.accumulatorBramIOs(a).dataIn_1.poke(doubleToUInt(rowValue1))
            c.io.accumulatorBramIOs(a).write_1.poke(i < numberOfNonZeros)
            c.io.accumulatorBramIOs(a).addr_2.poke(offset + i + 1)
            c.io.accumulatorBramIOs(a).dataIn_2.poke(doubleToUInt(rowValue2))
            c.io.accumulatorBramIOs(a).write_2.poke(i + 1 < numberOfNonZeros)
          }

          c.clock.step()
        }
      }
    }

    for (color <- 0 until colors) {

      for (r <- 0 until rowsPerColorPerAccumulator) {

        val rowIndices = ListBuffer.fill(numberOfAccumulators)(0)
        val offset = 3 + colors + 3 * totalRowsPerAccumulator +
          (rowsPerColorPerAccumulator * color + r) * numberOfNonZerosPerRow + totalNonZerosPerAccumulator

        for (a <- 0 until numberOfAccumulators) {

          rowIndices(a) = r + a * rowsPerColorPerAccumulator

          val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
          val columnIndices = columnIndicesArray(color)(rowIndices(a))

          for (i <- 0 until numberOfNonZeros)
            columnIndices += (i + (r + a * rowsPerColorPerAccumulator) * numberOfNonZerosPerRow)
        }

        for (i <- 0 until numberOfNonZerosPerRow by 2) {

          for (a <- 0 until numberOfAccumulators) {

            val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
            val columnIndices = columnIndicesArray(color)(rowIndices(a))

            val columnIndex1 = if (i < numberOfNonZeros) columnIndices(i) else 0
            val columnIndex2 = if (i + 1 < numberOfNonZeros) columnIndices(i + 1) else 0

            c.io.accumulatorBramIOs(a).addr_1.poke(offset + i)
            c.io.accumulatorBramIOs(a).dataIn_1.poke(columnIndex1)
            c.io.accumulatorBramIOs(a).write_1.poke(i < numberOfNonZeros)
            c.io.accumulatorBramIOs(a).addr_2.poke(offset + i + 1)
            c.io.accumulatorBramIOs(a).dataIn_2.poke(columnIndex2)
            c.io.accumulatorBramIOs(a).write_2.poke(i + 1 < numberOfNonZeros)
          }

          c.clock.step()
        }

        for (a <- 0 until numberOfAccumulators) {

          val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
          val rowValues = rowValuesArray(color)(rowIndices(a))
          val columnIndices = columnIndicesArray(color)(rowIndices(a))
          val diagonalIndex = diagonalIndices(color)(rowIndices(a))
          val initSum = initSums(color)(rowIndices(a))

          c.io.accumulatorBramIOs(a).write_1.poke(false)
          c.io.accumulatorBramIOs(a).write_2.poke(false)

          diagonalColumnIndices(color)(rowIndices(a)) = columnIndices.remove(diagonalIndex)
          columnIndicesArray(color)(rowIndices(a)) = columnIndices

          println("numberOfNonZeros: " + numberOfNonZeros)
          println("diagonalValueIndex: " + diagonalIndex)
          println("diagonalValue: " + diagonalValues(color)(rowIndices(a)) + " " + doubleToUInt(diagonalValues(color)(rowIndices(a))))
          println("diagonalValueColumnIndex: " + diagonalColumnIndices(color)(rowIndices(a)))
          println("initSum: " + initSum + " " + doubleToUInt(initSums(color)(rowIndices(a))))
          println("rowValues (double): " + rowValues)
          println("rowValues (int)  : " + rowValues.map(doubleToUInt))
          println("columnIndices: " + columnIndices)
          println()
        }
      }
    }

    //calculate results
    if (backwards) {

      for (color <- colors - 1 to 0 by -1) {

        for (r <- rowsPerColorPerAccumulator - 1 to 0 by -1) {

          val rowIndices = ListBuffer.fill(numberOfAccumulators)(0)

          for (a <- numberOfAccumulators - 1 to 0 by -1) {

            rowIndices(a) = r + a * rowsPerColorPerAccumulator

            val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
            val rowValues = rowValuesArray(color)(rowIndices(a))
            val columnIndices = columnIndicesArray(color)(rowIndices(a))
            var initSum = initSums(color)(rowIndices(a))
            var sum2 = 0d

            for (i <- numberOfNonZeros - 2 to 0 by -1) {

              val temp = sum2
              sum2 = initSum - (rowValues(i) * resultVariables(columnIndices(i)))
              initSum = temp
            }

            resultVariables(diagonalColumnIndices(color)(rowIndices(a))) = 1 / diagonalValues(color)(rowIndices(a)) * (initSum + sum2)
          }
        }
      }
    } else {

      for (color <- 0 until colors) {

        for (r <- 0 until rowsPerColorPerAccumulator) {

          val rowIndices = ListBuffer.fill(numberOfAccumulators)(0)

          for (a <- 0 until numberOfAccumulators) {

            rowIndices(a) = r + a * rowsPerColorPerAccumulator

            val numberOfNonZeros = numberOfNonZerosArray(color)(rowIndices(a))
            val rowValues = rowValuesArray(color)(rowIndices(a))
            val columnIndices = columnIndicesArray(color)(rowIndices(a))
            var initSum = initSums(color)(rowIndices(a))
            var sum2 = 0d

            for (i <- 0 until numberOfNonZeros - 1) {

              val temp = sum2
              sum2 = initSum - (rowValues(i) * resultVariables(columnIndices(i)))
              initSum = temp
            }

            resultVariables(diagonalColumnIndices(color)(rowIndices(a))) = 1 / diagonalValues(color)(rowIndices(a)) * (initSum + sum2)
          }
        }
      }
    }

    c.clock.step()

    println("Starting execution")

    c.io.cmd.valid.poke(true)
    c.clock.step()
    c.io.cmd.valid.poke(false)

    while (c.io.busy.peekBoolean()) c.clock.step()

    //to ensure all writes have been done
    c.clock.step(10)

    verifyResults(resultVariables, c)

    println("\nSuccess!")
  }

  def writeVariablesOnMem(variables: ListBuffer[scala.Double], c: SyGSPETestModule): Unit = {

    println("Writing variables on memory\n")
    for(i <- variables.indices by c.variablesMemBanks * 2) {

      val indicesRange = Seq.range(i, Math.min(i + c.variablesMemBanks * 2, variables.size))
      val bankIndices = indicesRange.map(index => index % c.variablesMemBanks)
      val variablesToWrite = indicesRange.map(index => variables(index))

      bankIndices.zipWithIndex.foreach { case (bankIndex, k) =>

        val variable = variablesToWrite(k)
        if(k < c.variablesMemBanks) {

          c.io.variableBramIOs(bankIndex).addr_1.poke(indicesRange(k) / c.variablesMemBanks)
          c.io.variableBramIOs(bankIndex).write_1.poke(true)
          c.io.variableBramIOs(bankIndex).dataIn_1.poke(doubleToUInt(variable))
        }
        else {

          c.io.variableBramIOs(bankIndex).addr_2.poke(indicesRange(k) / c.variablesMemBanks)
          c.io.variableBramIOs(bankIndex).write_2.poke(true)
          c.io.variableBramIOs(bankIndex).dataIn_2.poke(doubleToUInt(variable))
        }
      }

      c.clock.step()

      bankIndices.foreach { index =>

        c.io.variableBramIOs(index).write_1.poke(false)
        c.io.variableBramIOs(index).write_2.poke(false)
      }
    }
    println("variables (double): " + variables)
    println("variables (int)  : " + variables.map(v => doubleToUInt(v)))
    println()
  }

  def verifyResults(resultVariables: ListBuffer[scala.Double], c: SyGSPETestModule): Unit = {

    println("\nVerifying results")
    for(i <- resultVariables.indices by c.variablesMemBanks * 2) {

      val indicesRange = Seq.range(i, Math.min(i + c.variablesMemBanks * 2, resultVariables.size))
      val bankIndices = indicesRange.map(index => index % c.variablesMemBanks)
      val resultsToCheck = indicesRange.map(index => resultVariables(index))

      bankIndices.zipWithIndex.foreach { case (bankIndex, k) =>

        //println(indicesRange(k) + " " + bankIndex + " " + indicesRange(k) / c.variablesMemBanks)
        if(k < c.variablesMemBanks) {

          c.io.variableBramIOs(bankIndex).addr_1.poke(indicesRange(k) / c.variablesMemBanks)
          c.io.variableBramIOs(bankIndex).enable_1.poke(true)
        }
        else {

          c.io.variableBramIOs(bankIndex).addr_2.poke(indicesRange(k) / c.variablesMemBanks)
          c.io.variableBramIOs(bankIndex).enable_2.poke(true)
        }
      }

      c.clock.step()

      bankIndices.zipWithIndex.foreach { case (bankIndex, k) =>

        val result = resultsToCheck(k)
        if(k < c.variablesMemBanks) {

          c.io.variableBramIOs(bankIndex).dataOut_1.expect(doubleToUInt(result))
          c.io.variableBramIOs(bankIndex).enable_1.poke(false)
        }
        else {

          c.io.variableBramIOs(bankIndex).dataOut_2.expect(doubleToUInt(result))
          c.io.variableBramIOs(bankIndex).enable_2.poke(false)
        }
      }
    }
  }
}
