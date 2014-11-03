import scala.collection.immutable.Vector

class ProngPos(val num: Int)
{
  def letter: Char = ProngPos.letter(num)

  override def toString: String = "ProngPos " + letter
}

object ProngPos
{
  val letter: Vector[Char] = Vector('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

  // The grids for each prong position.  Each such grid has exactly
  // one true.
  val bitGrid: Vector[Vector[Vector[Boolean]]] =
    // Start with pairs of coordinates of nonfalse entry for each
    // prong position
    Vector((0,0), (0,1), (1,1), (1,0),
           (1,-1), (0,-1), (-1,-1),
           (-1,0), (-1,1)).
      map(buildArray)

  private def buildArray(trueDeltaXY: (Int,Int)):Vector[Vector[Boolean]] =
    trueDeltaXY match
    {
      case (trueDeltaX, trueDeltaY) =>
        Vector.tabulate(Size, Size)((i:Int ,j:Int) =>
                                      {
                                        val (deltaX, deltaY) =
                                          subscriptsToOffsets(i,j)
                                        deltaX == trueDeltaX &&
                                          deltaY == trueDeltaY
                                      })
    }

  def subscriptsToOffsets(i: Int, j: Int):(Int,Int) = (j - 1, i - 1)
}
