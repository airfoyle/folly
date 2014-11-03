class Position(val toMove: Player,
               // Vector of row vectors
               val board: Vector[Vector[PodStack]],
               val reserves: Vector[PlayerResources], // One per player
               val game: OctiGame)

////               val podReserve: Vector[Int],   // These are arrays because
////               val prongReserve: Vector[Int], // there's one per player


object Position
{
  private def holdsPods(colOffset:Int):Boolean =
     (colOffset / OctiConst.HomeSpacing < OctiConst.initNumPods) &&
     (colOffset % OctiConst.HomeSpacing == 0)

  def initial(game: OctiGame): Position =
    {
      val board: Vector[Vector[PodStack]] = 
         (Vector.tabulate[PodStack]
                (game.rules.boardHeight, game.rules.boardWidth)
                ((row:Int, col:Int) =>
                 {
                   val loc = new Podloc(row, col);
                   val x =
                     (0 to game.rules.numPlayers).
                       find((i) =>
                            (game.homeSquares[i].contains(loc)));
                   x match
                   {
                     case Some(i) => new PodStack(game.players(i))
                     case None => null
                   }
                 }))
      new Position(game.first,
                   board,
                   PlayerResources.initial(game),
                   game)
    }

////      val podReserve: Vector[Int] = 
////           Vector.fill[Int](OctiConst.NumPlayers)
////                 ( => OctiConst.InitPodReserve)
////      val prongReserve: Vector[Int] =
////           Vector.fill[Int](OctiConst.NumPlayers)
////                 ( => OctiConst.InitProngReserve)

    
 }
