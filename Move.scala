import ScalaUtils._

// An Octi move. All the interesting stuff happens in the subclasses
// of Move.
abstract class Move(loc: Podloc)
{
  // -- loc is where move occurs, either arrival point (for Add and Rescue),
  // point where pod sits (for Insert and Shift), or point where
  // pod starts from (for Motion)

   def isLegal(pos: Position): Boolean;

   // Given that it's legal, execute the move --
   def execute(pos: Position): Position;

   def allLegalMoves : List[Move]
}

trait MoveGenerator
{
  def allLegalIn[T <: Move](pos: Position): List[T];
}



