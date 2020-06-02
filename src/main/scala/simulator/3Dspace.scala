package simulator

import scala.math._

object Pos {
  def apply(x: Double, y: Double, z: Double) = new Pos(x, y, z)
  def apply(r: Vector[Double]) = new Pos(r(0), r(1), r(2))
  val O = new Pos(0.0, 0.0, 0.0)
}
class Pos(val x: Double, val y: Double, val z: Double){
  def apply(): Vector[Double] = Vector(x, y, z) //Apply method returns position vector
  def ==(other: Pos): Boolean = (this.x, this.y, this.z) == (other.x, other.y, other.z)
  def !=(other: Pos): Boolean = (this.x, this.y, this.z) != (other.x, other.y, other.z)
  def d(other: Pos): Double = sqrt(pow(other.x-this.x, 2) + pow(other.y-this.y, 2) + pow(other.z-this.z, 2))

  override def toString() = s"(${this.x}, ${this.y}, ${this.z})"
}
