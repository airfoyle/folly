import scala.collection.immutable.{Vector,Set}

//In addition to the eponymous OctiGame, this file defines the
//classes OctiRules and MoveRes; see below.

class OctiGame(val rules: OctiRules,
               val players: Vector[Player],
               val first: Player)
{
  // In reverse-chrono order.  Alternating by player.
  private var moves: List[MoveRes];

  if (players.length != rules.numPlayers)
    throw new OctiGame.InvalidNumberOfPlayers(rules.numPlayers,
                                              players.length);

  def moveAdd(m: Move) { moves +: m }

  def getMoves() = moves.reverse

  def boardHeight() = rules.boardHeight()

  def boardWidth() = rules.boardWidth()
}

object OctiGame
{
  class InvalidNumberOfPlayers(required: Int, tried: Int) extends
     Exception("""Wrong number of players for game:
Rules require ${required}, tried ${tried}""")

  val standardHomeSquares =
    Vector(2,6).map
       (row = >
         ((2 to 6 by 2).toSet.map
           (col => new Podloc(col, row))))

}

// Describes which rules we're operating under
class OctiRules(val numPlayers: Int = 2,
                val boardWidth: Int,
                val boardHeight: Int,
                val homeSquares: Vector[Set[Podloc]],
                val podReserve: Int,
                val prongReserve: Int,
                val allowStacking: Boolean,
                // If allowRescue == true, then you can place captured
                // OR reserve pods on occupied enemy squares --
                val allowRescue: Boolean = false,
                // If takeAllToWin == true, you have to capture
                // all enemy squares to win.
                val takeAllToWin: Boolean,
                val wrapAround: Boolean = false);

object OctiJunior
   extends OctiRules(boardWidth = 6, boardHeight = 7,
                     homeSquares =
                       Vector(1,5).
                         map(row =>
                              ((1 to 4).toSet.map
                                 (col => new Podloc(col, row)))),
                     podReserve = 0,
                     prongReserve = 12,
                     allowStacking = false,
                     takeAllToWin = false);

object OctiXFast
  extends OctiRules(boardWidth = 9, boardHeight = 9,
                    OctiGame.standardHomeSquares,
                    podReserve = 4,
                    prongReserve = 25,
                    allowStacking = true,
                    allowRescue = false,
                    takeAllToWin = false)

object OctiX
  extends OctiRules(boardWidth = 9, boardHeight = 9,
                    OctiGame.standardHomeSquares,
                    podReserve = 4,
                    prongReserve = 25,
                    allowStacking = true,
                    allowRescue = true,
                    takeAllToWin = true)

// To extend to 4-player game, you need another slot, allies,
// a list of Ints specifying which other players are on your side.

/** Records a move and the position it created.  move is null
  * only if res is the initial position.
  */
case class MoveRes(move: Move, res: Position)
