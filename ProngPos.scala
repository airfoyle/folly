import scala.collection.immutable.Vector

class ProngPos(val num: Int)
{
  def letter: Char = ProngPos.letter(num)

  override def toString: String = "ProngPos " + letter
}

object ProngPos
{
  val letter: Vector[Char] = Vector('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
}




//// object ProngPosTest extends App
//// {
//// 
//// class ProngPos extends Enumeration
//// {
////   type ProngPos = Value
////   val A,B,C,D,E,F,G,H = Value
//// }
//// 
////   println("Maximum ProngPos: " + ProngPos.maxId)
//// }
