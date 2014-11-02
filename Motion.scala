class MotionSet(motions: Set[Motion], loc: Podloc) extends Move(loc)
{
  def isLegal(pos: Position)
  {

  }
}


class Motion(val dest: Podloc);

object Motion
{

}

case class Slide(val dest: Podloc) extends Motion(dest)
{
}


case class Jump(val waypoints: List[Podloc],
                val captures: List[PodStack],
                val dest: PodLoc) extends(Motion dest)
{
}
