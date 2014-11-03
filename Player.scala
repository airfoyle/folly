import java.awt.Color

// Octi player
class Player(val index: Int, name: String, color: java.awt.Color)
{
  // Hmm.  Only meaningful for two-player game --
  def opponent(game: OctiGame): Player =
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

  private val saturation: Float = 0.25F
  private val brightness: Float = 0.8F

  val Colors = Vector(Color.getHSBColor(0.2F, saturation, brightness),
                      Color.getHSBColor(0.4F, saturation, brightness),
                      Color.getHSBColor(0.6F, saturation, brightness),
                      Color.getHSBColor(0.8F, saturation, brightness))

  val Self = new Player(myIndex, "Folly", Colors(myIndex))

}
