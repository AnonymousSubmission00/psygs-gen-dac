package spatial_templates

import chisel3._
import chisel3.util._
import hardfloat._

import scala.language.implicitConversions

//Bundles that represent the raw bits of custom hardware datatypes
case class Float(expWidth: Int, sigWidth: Int) extends Bundle {

  val bits: UInt = UInt((expWidth + sigWidth).W)
  val bias: Int = (1 << (expWidth - 1)) - 1
}

case object Float32 {

  def apply(): Float = Float(8, 24)
}

case object Float64 {

  def apply(): Float = Float(11, 53)
}

//The Arithmetic typeclass which implements various arithmetic operations on custom hardware datatypes
abstract class Arithmetic[T <: Data] {

  implicit def cast(t: T): ArithmeticOps[T]
}

abstract class ArithmeticOps[T <: Data](self: T) {

  class DivisionResult(gen: T) extends DecoupledIO(gen) {

    val flags: UInt = Output(UInt())
  }

  def +(t: T): T
  def -(t: T): T
  def *(t: T): T

  /**
   * Instantiates a new divider to perform self / denom
   * @param denom      the denominator of the division
   * @param inputValid true when both numerator and denominator are valid, false otherwise
   * @return A decoupled signal containing:
   *         . the output of the divider as bits
   *         . the ready and valid signals of the divider
   *         . the flags of the divider
   */
  def /(denom: T, inputValid: Bool): DivisionResult

  def mac(m1: T, m2: T): T                              //Returns (m1 * m2 + self)
  def >>(u: UInt): T                                    //This is a rounding shift! Rounds away from 0
  def >(t: T): Bool
  def identity: T
  def withWidthOf(t: T): T
  def clippedToWidthOf(t: T): T                         //Like "withWidthOf", except that it saturates
  def relu: T
  def zero: T
  //def relu6(shift: UInt): T
  //def minimum: T

  //Optional parameters, which only need to be defined if you want to enable various optimizations for transformers
  def sqrt: Option[(DecoupledIO[UInt], DecoupledIO[T])] = None
  def reciprocal[U <: Data](u: U): Option[(DecoupledIO[UInt], DecoupledIO[U])] = None
  def mult_with_reciprocal[U <: Data](reciprocal: U): T = self
}

object Arithmetic {

implicit object UIntArithmetic extends Arithmetic[UInt] {
    override implicit def cast(self: UInt) = new ArithmeticOps(self) {
      override def *(t: UInt) = self * t
      override def mac(m1: UInt, m2: UInt) = m1 * m2 + self
      override def +(t: UInt) = self + t
      override def /(denom: UInt, inputValid: Bool) = {
        val out = Wire(new DivisionResult(UInt(32.W)))
        out
      }
      override def -(u: UInt) = {
          val out = Wire(u)
          out
      }

      override def >>(u: UInt) = {
        // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

        // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
        val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
        val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
        val ones_digit = self(u)

        val r = point_five & (zeros | ones_digit)

        (self >> u).asUInt() + r
      }

      override def >(t: UInt): Bool = self > t

      override def withWidthOf(t: UInt) = self.asTypeOf(t)

      override def clippedToWidthOf(t: UInt) = {
        val sat = ((1 << (t.getWidth-1))-1).U
        Mux(self > sat, sat, self)(t.getWidth-1, 0)
      }

      override def relu: UInt = self
      /*override def relu6(shift: UInt): UInt = {
        val max6 = (6.U << shift).asUInt()
        val maxwidth = ((1 << (self.getWidth-1))-1).U
        val max = Mux(max6 > maxwidth, maxwidth, max6)(self.getWidth-1, 0).asUInt()
        Mux(self < max, self, max)
      }*/

      override def zero: UInt = 0.U
      override def identity: UInt = 1.U
    }
  }

  implicit object SIntArithmetic extends Arithmetic[SInt] {
      override implicit def cast(self: SInt) = new ArithmeticOps(self) {
        override def *(t: SInt) = self * t
        override def mac(m1: SInt, m2: SInt) = m1 * m2 + self
        override def +(t: SInt) = self + t
        override def /(denom: SInt, inputValid: Bool) = {
          val out = Wire(new DivisionResult(SInt(32.W)))
          out
        }
        override def -(u: SInt) = {
          val out = Wire(u)
          out
        }
        override def >>(u: UInt) = {
          // The equation we use can be found here: https://riscv.github.io/documents/riscv-v-spec/#_vector_fixed_point_rounding_mode_register_vxrm

          // TODO Do we need to explicitly handle the cases where "u" is a small number (like 0)? What is the default behavior here?
          val point_five = Mux(u === 0.U, 0.U, self(u - 1.U))
          val zeros = Mux(u <= 1.U, 0.U, self.asUInt() & ((1.U << (u - 1.U)).asUInt() - 1.U)) =/= 0.U
          val ones_digit = self(u)

          val r = (point_five & (zeros | ones_digit)).asBool()

          (self >> u).asSInt() + Mux(r, 1.S, 0.S)
        }

        override def >(t: SInt): Bool = self > t

        override def withWidthOf(t: SInt) = {
          if (self.getWidth >= t.getWidth)
            self(t.getWidth-1, 0).asSInt()
          else {
            val sign_bits = t.getWidth - self.getWidth
            val sign = self(self.getWidth-1)
            Cat(Cat(Seq.fill(sign_bits)(sign)), self).asTypeOf(t)
          }
        }

        override def clippedToWidthOf(t: SInt): SInt = {
          val maxsat = ((1 << (t.getWidth-1))-1).S
          val minsat = (-(1 << (t.getWidth-1))).S
          MuxCase(self, Seq((self > maxsat) -> maxsat, (self < minsat) -> minsat))(t.getWidth-1, 0).asSInt()
        }

        override def relu: SInt = Mux(self >= 0.S, self, 0.S)
        /*override def relu6(shift: UInt): SInt = {
          val max6 = (6.S << shift).asSInt()
          val maxwidth = ((1 << (self.getWidth-1))-1).S
          val max = Mux(max6 > maxwidth, maxwidth, max6)(self.getWidth-1, 0).asSInt()
          MuxCase(self, Seq((self < 0.S) -> 0.S, (self > max) -> max))
        }*/

        override def zero: SInt = 0.S
        override def identity: SInt = 1.S
      }
    }

  implicit object FloatArithmetic extends Arithmetic[Float] {

    //TODO Floating point arithmetic currently switches between recoded and standard formats for every operation.
    // However, it should stay in the recoded format as it travels through the systolic array

    override implicit def cast(self: Float): ArithmeticOps[Float] = new ArithmeticOps(self) {

      override def +(t: Float): Float = {

        require(self.getWidth >= t.getWidth) //This just makes it easier to write the resizing code

        //Recode all operands
        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        //Generate 1 as a float
        val in_to_rec_fn = Module(new INToRecFN(1, self.expWidth, self.sigWidth))
        in_to_rec_fn.io.signedIn := false.B
        in_to_rec_fn.io.in := 1.U
        in_to_rec_fn.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        val one_rec = in_to_rec_fn.io.out

        //Resize t
        val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        //Perform addition
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := t_rec_resized
        muladder.io.b := one_rec
        muladder.io.c := self_rec

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        result
      }

      override def -(t: Float): Float = {

        val t_sgn = t.bits(t.getWidth-1)
        val neg_t = Cat(~t_sgn, t.bits(t.getWidth-2,0)).asTypeOf(t)
        self + neg_t
      }

      override def *(t: Float): Float = {

        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val t_resizer =  Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := t_rec_resized
        muladder.io.c := 0.U

        val out = Wire(Float(self.expWidth, self.sigWidth))
        out.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        out
      }

      override def /(denom : Float, inputValid: Bool): DivisionResult = {

        val denom_rec = recFNFromFN(denom.expWidth, denom.sigWidth, denom.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val denom_resizer = Module(new RecFNToRecFN(denom.expWidth, denom.sigWidth, self.expWidth, self.sigWidth))
        denom_resizer.io.in := denom_rec
        denom_resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        denom_resizer.io.detectTininess := consts.tininess_afterRounding
        val denom_rec_resized = denom_resizer.io.out

        val out = Wire(new DivisionResult(Float(self.expWidth, self.sigWidth)))

        if(self.expWidth == 11 && self.sigWidth == 53) {

          val divSqrt = Module(new DivSqrtRecF64())
          divSqrt.io.roundingMode := consts.round_near_even
          divSqrt.io.detectTininess := consts.tininess_afterRounding
          divSqrt.io.inValid := inputValid
          divSqrt.io.sqrtOp := false.B

          divSqrt.io.a := self_rec
          divSqrt.io.b := denom_rec_resized

          out.bits.bits := fNFromRecFN(self.expWidth, self.sigWidth, divSqrt.io.out)
          out.valid := divSqrt.io.outValid_div
          out.ready := divSqrt.io.inReady_div
          out.flags := divSqrt.io.exceptionFlags
        }
        else {

          val divSqrt = Module(new DivSqrtRecFN_small(self.expWidth, self.sigWidth, 0))
          divSqrt.io.roundingMode := consts.round_near_even
          divSqrt.io.detectTininess := consts.tininess_afterRounding
          divSqrt.io.inValid := inputValid
          divSqrt.io.sqrtOp := false.B

          divSqrt.io.a := self_rec
          divSqrt.io.b := denom_rec_resized

          out.bits.bits := fNFromRecFN(self.expWidth, self.sigWidth, divSqrt.io.out)
          out.valid := divSqrt.io.outValid_div
          out.ready := divSqrt.io.inReady
          out.flags := divSqrt.io.exceptionFlags
        }

        out
      }

      override def mac(m1: Float, m2: Float): Float = {

        //Recode all operands
        val m1_rec = recFNFromFN(m1.expWidth, m1.sigWidth, m1.bits)
        val m2_rec = recFNFromFN(m2.expWidth, m2.sigWidth, m2.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        //Resize m1 to self's width
        val m1_resizer = Module(new RecFNToRecFN(m1.expWidth, m1.sigWidth, self.expWidth, self.sigWidth))
        m1_resizer.io.in := m1_rec
        m1_resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        m1_resizer.io.detectTininess := consts.tininess_afterRounding
        val m1_rec_resized = m1_resizer.io.out

        //Resize m2 to self's width
        val m2_resizer = Module(new RecFNToRecFN(m2.expWidth, m2.sigWidth, self.expWidth, self.sigWidth))
        m2_resizer.io.in := m2_rec
        m2_resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        m2_resizer.io.detectTininess := consts.tininess_afterRounding
        val m2_rec_resized = m2_resizer.io.out

        //Perform multiply-add
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := m1_rec_resized
        muladder.io.b := m2_rec_resized
        muladder.io.c := self_rec

        //Convert result to standard format //TODO remove these intermediate recodings
        val out = Wire(Float(self.expWidth, self.sigWidth))
        out.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        out
      }

      override def >>(u: UInt): Float = {

        //Recode self
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        //Get 2^(-u) as a recoded float
        val shift_exp = Wire(UInt(self.expWidth.W))
        shift_exp := self.bias.U - u
        val shift_fn = Cat(0.U(1.W), shift_exp, 0.U((self.sigWidth-1).W))
        val shift_rec = recFNFromFN(self.expWidth, self.sigWidth, shift_fn)

        assert(shift_exp =/= 0.U, "scaling by denormalized numbers is not currently supported")

        //Multiply self and 2^(-u)
        val muladder = Module(new MulAddRecFN(self.expWidth, self.sigWidth))

        muladder.io.op := 0.U
        muladder.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        muladder.io.detectTininess := consts.tininess_afterRounding

        muladder.io.a := self_rec
        muladder.io.b := shift_rec
        muladder.io.c := 0.U

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := fNFromRecFN(self.expWidth, self.sigWidth, muladder.io.out)
        result
      }

      override def >(t: Float): Bool = {

        //Recode all operands
        val t_rec = recFNFromFN(t.expWidth, t.sigWidth, t.bits)
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        //Resize t to self's width
        val t_resizer = Module(new RecFNToRecFN(t.expWidth, t.sigWidth, self.expWidth, self.sigWidth))
        t_resizer.io.in := t_rec
        t_resizer.io.roundingMode := consts.round_near_even
        t_resizer.io.detectTininess := consts.tininess_afterRounding
        val t_rec_resized = t_resizer.io.out

        val comparator = Module(new CompareRecFN(self.expWidth, self.sigWidth))
        comparator.io.a := self_rec
        comparator.io.b := t_rec_resized
        comparator.io.signaling := false.B

        comparator.io.gt
      }

      override def withWidthOf(t: Float): Float = {

        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth))
        result.bits := fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out)
        result
      }

      override def clippedToWidthOf(t: Float): Float = {

        //TODO check for overflow. Right now, we just assume that overflow doesn't happen
        val self_rec = recFNFromFN(self.expWidth, self.sigWidth, self.bits)

        val resizer = Module(new RecFNToRecFN(self.expWidth, self.sigWidth, t.expWidth, t.sigWidth))
        resizer.io.in := self_rec
        resizer.io.roundingMode := consts.round_near_even //consts.round_near_maxMag
        resizer.io.detectTininess := consts.tininess_afterRounding

        val result = Wire(Float(t.expWidth, t.sigWidth))
        result.bits := fNFromRecFN(t.expWidth, t.sigWidth, resizer.io.out)
        result
      }

      override def relu: Float = {

        val raw = rawFloatFromFN(self.expWidth, self.sigWidth, self.bits)

        val result = Wire(Float(self.expWidth, self.sigWidth))
        result.bits := Mux(!raw.isZero && raw.sign, 0.U, self.bits)
        result
      }

      override def zero: Float = 0.U.asTypeOf(self)
      override def identity: Float = Cat(0.U(2.W), ~(0.U((self.expWidth-1).W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
      def minimum: Float = Cat(1.U, ~(0.U(self.expWidth.W)), 0.U((self.sigWidth-1).W)).asTypeOf(self)
    }
  }
}
