import scala.collection.immutable.BitSet

/** Represents a set of possible directions for a pod to move.
  */
class PodPower(val bits: BitSet = BitSet())
{
  // The bits in val bits are the positions occupied by prongs,
  // numbered thus: start with 1 in the north point of the compass
  // and work clockwise to 8, pointing northwest.  Letter codes
  // for those direction are A through H.
  // A pod with bit i set can move in direction i.

  // No-arg constructor makes a pod that can't move anywhere --
  def this() = this(BitSet())

  // Grids are 3x3 representations of legal x,y offsets (-1, 0, or 1) 
  // v(i)(j) == true iff (j-1,i-1) is a legal offset.  
  // Note that v(1)(1) is a "hole": (0,0) is never legal as
  // a direction to move.  But we count it as one for technical
  // purposes.
  val deltaGrid: Vector[Vector[Boolean]] =
     bits.map((x: Int) => ProngPos.bitGrid(x)).
           reduceLeft(ScalaUtils.booleVecTwoOr)

  // Does the power include direction i?  The 0 direction is included
  // in every power.
  def includes(i: Int) = i== 0 || bits(i)

  def addProng(i: Int):PodPower =
    new PodPower(bits + i)

}

object PodPower
{
  // Grid of offsets is 3x3 --
  val Size = 3
}
