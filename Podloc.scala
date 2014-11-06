case class Podloc(val x:Int, val y:Int)
{
  // -- x is column, y is row

  def onBoard(game: OctiGame): Boolean =
  {
    (x >= 0 && x < game.boardWidth &&
     y >= 0 && y < game.boardHeight)
  }

  // If loc is on board, and delta steps away in some direction
  // return that direction, else None.  The game must be supplied
  // to give the board dimensions and whether wrap-around moves
  // are legal.
  def nearDir(loc: Podloc, near: Int, game: Octigame): Option(ProngPos) =
  {
    if (loc.onBoard(game))
      {
        def nearDelta(deltaX: Int, deltaY: Int):Boolean =
          {
            Math.abs(deltaX) == near && deltaY == 0 ||
            deltaX == 0 && Math.abs(deltaY) == near ||
            Math.abs(deltaX) == near && Math.abs(deltaY) == near
          }

        val deltaX = loc.x - x;
        val deltaY = loc-y - y;
        if (nearDelta(deltaX, deltaY))
          {
            Some(ProngPos.dirFor(deltaX / near, deltaY / near))
          }
          else if (game.rules.wrapAround)
            {
              // Boost deltaX and deltaY into the positive realm,
              // find the remainder modulo boardWidth, and try again --
              val deltaX2 = (deltaX + game.boardWidth) % game.boardWidth;
              val deltaY2 = (deltaY + game.boardHeight) % game.boardHeight;
              if (nearDelta(deltaX2, deltaY2))
                Some(ProngPos.dirFor(deltaX2 / near, deltaY2 / near))
              else None
            }
          else None
      }
      else None
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
