import java.awt.Color

// Octi player
class Player(val index: Int, name: String, color: java.awt.Color)
{
  // Hmm.  Only meaningful for two-player game --
  def opponent: Player(game: OctiGame) =
    game.players(1-index)
   // Player.getPlayer(if (i == 0) Player.One else Player.Zero)
}

object Player
{
  // The index of the computer, considered as a player.
  // It's not really the computer, it's more like "the Octi
  // program."  Name?  Folly? LLC (Late-Life Crisis)?
  val myIndex = 0

  // Functional Overexuberance due to Late-Life Youthfulness

  private val saturation = 0.25
  private val brightness = 0.8

  val Colors = Vector(Color.getHSBColor(0.2, saturation, brightness),
                      Color.getHSBColor(0.4, saturation, brightness),
                      Color.getHSBColor(0.6, saturation, brightness),
                      Color.gethsbcolor(0.8, saturation, brightness))

  val Self = new Player(myIndex, "Folly", Colors(myIndex))

}
