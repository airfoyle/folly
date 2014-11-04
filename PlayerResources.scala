import scala.collection.immutable.Vector

class PlayerResources(val podReserve: Int, val prongReserve: Int)
{
  def podPlus(delta: Int): PlayerResources =
    new PlayerResources(podReserve + delta, prongReserve)

  def prongPlus(delta: Int): PlayerResources =
    new PlayerResources(podReserve, prongReserve + delta)
}

object PlayerResources
{
  def initial(game: OctiGame): Vector[PlayerResources] =
    (((0 until (game.rules.numPlayers)).map
         ((i: Int) => new PlayerResources(game.rules.podReserve,
                                          game.rules.prongReserve))).
      toVector)
}
