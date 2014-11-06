import scala.collection.immutable.Vector

/** A prong position, a number from 1 to OctiConst.NumDirs representing
  * a direction a pod could go.
  */
class ProngPos(val num: Int)
{
  val letter: Char = ProngPos.letter(num - 1)

  override def toString: String = "ProngPos " + letter
}

object ProngPos
{
  val min = 1
  val max = OctiConst.NumDirs

  val letter: Vector[Char] = Vector('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')

  val forDelta: Vector[Vector[ProngPos]] =
    Vector(Vector(8, 1, 2),
           Vector(7, 0, 3),
           Vector(6, 5, 4))

  // The grids for each prong position.  Each such grid has exactly
  // one true.
  val bitGrid: Vector[Vector[Vector[Boolean]]] =
    // Start with pairs of coordinates of nonfalse entry for each
    // prong position
    Vector((0,0), (0,1), (1,1), (1,0),
           (1,-1), (0,-1), (-1,-1),
           (-1,0), (-1,1)).
      map(buildArray)

  /** The number of rows and columns in the inner two dimensions of
    * bitGrid.  bitGrid[p] is a 3x3 array giving the direction
    * direction p allows movement in.
    */
  private val Size = 3;

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

  /** Converts subscripts in a bitGrid array to the corresponding
    * directions on the board.  Scala only allows subscripts starting
    * from 0, or we would have used -1:1 in the first place.
    */
  def subscriptsToOffsets(i: Int, j: Int): (Int,Int) = (j - 1, i - 1)

  /** Converts directions on the board to subscripts in the
    * bitGrid and forDelta arrays.
    */
  def offsetsToSubscripts(deltaX: Int, deltaY: Int): (Int,Int) =
    (deltaX + 1, deltaY + 1)
}
