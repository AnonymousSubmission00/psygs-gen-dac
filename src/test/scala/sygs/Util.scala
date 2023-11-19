package sygs

import chisel3._
import chiseltest._
object Util {

  def intToUInt(i: Int): UInt = {

    val x = BigInt(i.toBinaryString, 2)
    x.U
  }

  def longToUInt(i: Long): UInt = {

    val x = BigInt(i.toBinaryString, 2)
    x.U
  }

  def floatToUInt(f: java.lang.Float): UInt = {

    val x = java.lang.Float.floatToIntBits(f)

    val bit0 = x & 0x1
    val xUnsigned = (BigInt((x >> 1) & 0x7FFFFFFF) << 1) + BigInt(bit0)
    xUnsigned.U(32.W)
  }

  def doubleToUInt(f: scala.Double): UInt = {

    val x = java.lang.Double.doubleToLongBits(f)

    val bit0 = x & 0x1
    val xUnsigned = (BigInt((x >> 1) & 0x7FFFFFFFFFFFFFFFL) << 1) + BigInt(bit0)
    xUnsigned.U(64.W)
  }

  def bigIntToFloat(x: BigInt): java.lang.Float = {

    val bit0 = x & 0x1
    val xSigned = (((x >> 1) & 0x7FFFFFFF) << 1) + bit0
    java.lang.Float.intBitsToFloat(xSigned.intValue)
  }

  def bigIntToDouble(x: BigInt): java.lang.Double = {

    val bit0 = x & 0x1
    val xSigned = (((x >> 1) & 0x7FFFFFFFFFFFFFFFL) << 1) + bit0
    java.lang.Double.longBitsToDouble(xSigned.longValue)
  }

  def check(wire: Bits, expected: UInt) {

    val value = wire.peek()
    assert((value.litValue - expected.litValue).abs < 1000)
  }

  def checkFloat(wire: Bits, expected: Float) {

    val value = wire.peek()
    assert((bigIntToFloat(value.litValue) - expected).abs < 1e-40f)
  }

  def checkDouble(wire: Bits, expected: Double) {

    val value = wire.peek()
    assert((bigIntToDouble(value.litValue) - expected).abs < 1000)
  }
}
