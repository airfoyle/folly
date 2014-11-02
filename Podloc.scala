case class Podloc(val x:Int, val y:Int)
{
  // -- x is column, y is row

  def onBoard(game: OctiGame): Boolean =
  {
    (x >= 0 && x < game.boardWidth &&
     y >= 0 && y < game.boardHeight)
  }

}

object Podloc
{
  class OffBoardException(x: Int, y: Int) extends
      Exception("Attempt to create pod location off board: [" +
                x + ", " + y + "]")

////   if (x < 1 || x > OctiConst.BoardWidth || y < 1 || y > OctiConst.BoardHeight)
////     throw new OffBoardException();

}
