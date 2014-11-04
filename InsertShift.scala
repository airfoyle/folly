// You can't distinguish among pods of equivalent types.
class Insert(prong: ProngPos, pod: PodPower, loc: Podloc) extends Move(loc)
{
  def isLegal(pos: Position) =
    {
      val player = pos.toMove
      val prongNum = pos.prongReserve(player.index)
      // There have to be prongs to insert and holes to insert them in ...
      prongNum > 0 && !(pod.includes(prongNum)) &&
        {
          // ... and there has to be one of that kind of pod
          // at the location
          val stack = pos.podsAt(loc)
          stack.includesOne(pod)
        }
    }

  def execute(pos:Position) =
    {
      val player = pos.toMove;
      val playerInd = playr.index;
      val newPodType = pod.addProng(prong);
      new Position(player.opponent,
                   vecTwoUpdated(pos.board,
                                 loc.y,
                                 loc.x,
                                 pos.podsAt(loc).
                                   entryPlus(pod, -1).
                                   entryPlus(newPodType, 1)),
                   pos.prongReserve.updated(playerInd,
                                            pos.prongReserve(playerInd)
                                                      - 1),
                   pos.podReserve,
                   pos.game)
    }
}

object Insert extends MoveGenerator
{
  def allLegal[Insert](pos: Position): List[Insert] =
    {
      val rules = pos.game.rules;
      val playerToMove = pos.toMove;
      if (pos.reserves[playerToMove.index].prongReserves > 0)
        {
          def podInserts(pp: PodPower, loc: Podloc): List[Insert] =
            {
              for (prongPos <- pp.bits)
                yield new Insert(prongPos, pp, loc)
            }

          for (x <- 0 until (rules.boardWidth);
               y <- 0 until (rules.boardHeight);
               stack = pos.board(y)(x);
               if (stack != null &&
                   stack.owner == playerToMove);
               (pp -> num) <- stack.pods;
               move <- podInserts(pp, Podloc(x,y)))
            yield move
      }.toList
    else List()
    }
}



class Shift(newhole: ProngPos, oldhole: Prongpos, loc: Podloc) 
    extends Move(loc)
{

}
