  def numberOf(pod: PodPower) = podMapNumberOf(pods, pod)
    

  // These seem like they should perhaps be somewhere else --

  private def minusOne(pm: ListMap[PodPower, Int], pod: PodPower):
      ListMap[PodPower, Int] = 
    {
       val numNow = numberOf(pod);
       if (numNow < 2)
         pm - PodPower
       else
         pm + (PodPower -> (numNow - 1))
    }

  private def podMapNumberOf(pm: ListMap[PodPower, Int], pod: PodPower):
      Int =
    pm.applyOrElse(pod, ((_) => 0))





////  def plus(kv: (A, Int)): PosIntMap[A] = kv match
////    {
////      case (k: A, v: Int) =>
////        val newNum = numberOf(x) + n;
////        if (newNum < 1)
////          ((this: ListMap - x): PosIntMap[A])
////        else
////          ((this: ListMap) + (x -> newNum)): PosIntMap[A]
////    }
////
////  def minusOne(x: A): PosIntMap[A] =
////    {
////       val numNow = numberOf(x);
////       if (numNow < 2)
////         this - x
////       else
////         this + (x -> (numNow - 1))
////    }


////    {
////      var m:PosIntMap[A] = new PosIntMap[A]()
////      for (p <- entries)
////        {
////          p match
////            {
////              case (x, n) => if (n > 0)
////                               {
////                                 m + (x -> n)
////                               }
////            }
////        }
////      m
////    }
