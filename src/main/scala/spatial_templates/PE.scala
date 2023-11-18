package spatial_templates

import chisel3._

/** Represents the ID of a PE/ME in the spatial template. It allows up to three
  * logical dimensions
  *
  * @param dim
  *   number of used dimension for this ID
  */
class ElemId(val dim: Int, val x: Int, val y: Int, val z: Int) {
  def printID() {
    print("D=" + dim + ", x=" + x + ", y=" + y + ", z=" + z + "\n")
  }
  def canEqual(a: Any) = a.isInstanceOf[ElemId]

  override def equals(o: Any): Boolean = {
    o match {
      case o: ElemId => {
        o.canEqual(this) &&
        (o.dim == dim) && (o.x == x) && (o.y == y) && (o.z == z)
      }
      case _ => false
    }
  }
}

/** This abstract class represents a Processing Element for a generic spatial
  * template
  */
abstract class PE(val id: ElemId) extends MultiIOModule
