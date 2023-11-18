package sygs
package pe.dfe.submodules

import chisel3._
import chisel3.util._
import java.io.File

class FloatAdderBlackBox64 extends HasBlackBoxPath {
  val io = IO(new Bundle {
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val pushin = Input(Bool())
    val r = Output(UInt(64.W))
    val pushout = Output(Bool())
  })

  addPath(new File("generators/spatial_templates/sygs/chisel/main/scala/pe/dfe/submodules/FloatAdderBlackBox64.v").getCanonicalPath)
}
