// This file defines Motion, a subclass of Move
// Slide and Jump are no longer subclasses of Move.

// A Move consists of a MotionSet, which is either all Slides or all
// Jumps (according to the rules --- I hope).

class MotionSet(motions: Set[PureMotion], loc: Podloc) extends Move(loc)
{
  def isLegal(pos: Position)
  {

  }
}


class Motion extends Move;

object Motion
{

}

/** The motion of a PodStack possibly split off from a bigger
  * stack.  It is not necessary to check if the stack is at
  * the starting point; that was already done, we'll suppose,
  * in the Motion class.  
  */
abstract class PureMotion(val mover: PodStack,
                          val start: Podloc,
                          val dest: Podloc)
{
  def isLegal(pos: Position): Boolean;
}



class Slide(val mover: PodStack,
            val start: Podloc,
            val dest: Podloc) extends
    Motion(mover, start, dest)
{
  def isLegal(pos: Position):
      Boolean =
    {
      
      dest.onBoard(pos.game) && dest.near(start, 1, pos.game)
      && dest.freeForPiece(mover.owner, pos.game)
    }
}


class Jump(val mover: PodStack,
           val start: Podloc,
           val waypoints: List[Podloc],
           val captures: List[PodStack],
           val dest: PodLoc) extends
    PureMotion(mover, start, dest)
{

}
