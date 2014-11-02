import scala.collection.immutable
import scala.collection.immutable.{Vector, List, Map, ListMap}

class PodStack(val owner: Player,
               val pods: MultiSet[PodPower] =
                 new MultiSet[PodPower](new PodPower() -> 1))
{
  // Map specifying for each sort of pod, how many we have in a pile --
  type PodSet = MultiSet[PodPower]    // ListMap[PodPower, Int]

  // pods Represents a multiset giving (positive) numbers of occurrences
  // of pods of a given power in this stack.  

  def includesOne(p: PodPower) = pods(p) > 0

  // Produce a list of all possible ways to divide into 8 or
  // fewer PodStacks to generate legal moves.  The vector 'legal'
  // describes which directions actually offer opportunities
  // for legal moves
  def divide(legal: Vector[Boolean]): List[Vector[PodStack]] =
    {
      val ways = PodStack.divide(pods, legal)
      val setVec =
        ways.map((w: Vector[PodSet]) =>
                 w.map((v) => new PodStack(owner, v)))
      setVec.toList
    }

  def minusOne(pod: PodPower) =
    new PodStack(owner, pods.entryPlus(pod, -1))

  def replace(podLeaving: PodPower, podArriving: PodPower) =
    new PodStack(owner,
                 pods.entryPlus(podLeaving, -1).entryPlus(podArriving, 1))
}

object PodStack
{
  // Map specifying for each sort of pod, how many we have in a pile --
  type PodSet = MultiSet[PodPower]

  private val emptyPodSet:PodSet = new MultiSet[PodPower]()

  import ScalaUtils._

  def empty(owner: Player):PodStack = 
    new PodStack(owner, emptyPodSet)

  // Find all possible ways to divide the stack into
  // piles.
  // A way is a vector of as many podpower maps as there are
  // directions.
  // On every recursive call we have a vector with more pods
  // until at the end we can return it.  Unless we can't
  // complete the current division, in which case we return nothing.
  def divide(pods: PodSet,
             legal: Vector[Boolean]):
       List[Vector[PodSet]] =
  {
    def len = legal.length

    // Find the possible tails of the vectors we're interested in --
    def startingFromPos(pods: PodSet, i: Int): List[List[PodSet]] =
      // This test is a correct termination test because
      // direction 0 is legal for all pods --
      if (lastLegalDirection(pods, legal, i))
        // There is no point in generating all the way to the end
        // in order to stack a proper subset of the pods on pile i if
        // the remainder won't go anywhere else --
        List(pods ::
             ((i+1).until(len).
              map((j) => emptyPodSet).toList))
      else if (legalDirection(pods, legal, i))
        {
          waysToCreateStack(i, pods).
            flatMap(fOpen2((podsHere:PodSet, podsOverThere:PodSet) =>
                            startingFromPos(podsOverThere, i+1).map
                              ((l: List[PodSet]) =>
                               podsHere :: l)))
////          for { (podsHere, podsOverThere) <- waysToCreateStack(i, pods)
////                 l <- startingFromPos(podsOverThere, i+1)
////             }
////          yield podsHere :: l
        }
      else for { l <- startingFromPos(pods, i+1) } yield
              emptyPodSet :: l

    startingFromPos(pods, 0).map((l) => l.toVector)
   }

  def waysToCreateStack(dir: Int, pods: PodSet):
          List[(PodSet, PodSet)] =
    {
      def stackSplit(pods: PodSet, pow: PodPower, n:Int):
            (PodSet, PodSet) =
        (pods.addInt(pow, n),
         pods.entryPlus(pow, -n))

      // Given one PodPower split into two piles stackN (kept here)
      // and stackNumsMinusN (distributed thither), figure out how
      // to combine that with ways to split the remaining PodPowers
      def splitsCombine(stackN: PodSet,
                        stackNumsMinusN: PodSet,
                        otherPods: PodSet):
          List[(PodSet, PodSet)] =
        (waysToCreateStack(dir, otherPods).
          map(fOpen2((podsHere: PodSet, podsOverThere: PodSet) =>
                     ((stackN ++ podsHere).
                           asInstanceOf[PodSet],
                       (stackNumsMinusN ++ podsOverThere).
                           asInstanceOf[PodSet]))))

      pods.headOption match
        {
          case Some((pow, num)) =>
             0.to(num).toList.flatMap(n =>
                                       {
                                          val (stackN, stackNumMinusN) =
                                               stackSplit(pods, pow, n);
                                          splitsCombine(stackN,
                                                        stackNumMinusN,
                                                        pods.tail)
                                       })
          case None => List((emptyPodSet, emptyPodSet))
        }
    }

  private def lastLegalDirection(pods: PodSet,
                                 legal: Vector[Boolean],
                                 i: Int):
              Boolean = 
  {
    legalDirection(pods, legal, i) &&
      (i+1).until(legal.length).
              forall((j) => !(legalDirection(pods, legal, j)))
  } 

  def legalDirection(pods: PodSet,
                     legal: Vector[Boolean],
                     dir: Int) =
    legal(dir) &&
      pods.exists{ case (pow, num) => pow.includes(dir) }
 
}
