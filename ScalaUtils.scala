import scala.collection.immutable.{ListMap, Vector}

object ScalaUtils
{
  def vecTwoUpdated[A](v: Vector[Vector[A]], i: Int, j: Int, newval: A)
  {
    v.updated(i, v(i).updated(j, newval))
  }

  def booleVecTwoOr(v1: Vector[Vector[Boolean]], v2: Vector[Vector[Boolean]]) =
  {
    val d1 = v1.length
    val d2 = v1(0).length
    if (v2.length != d1 || v2(0).length != d2)
      throw new TwoDimVecMismatch(v1, v2)
    Vector.tabulate(d1,d2)((i,j) => v1(i)(j) || v2(i)(j))
  }

  class TwoDimVecMismatch[A](val v1:Vector[Vector[A]],
                             val v2: Vector[Vector[A]])
      extends Exception("Two-dimensional vectors are of disparate dimensions")
  {
  }

  // Take a function of 2 arguments and turn it into a function of
  // one arguments, an ordered pair --
  def fOpen2[A,B,C](f: ((A, B) => C)):
      (((A,B)) => C) =
    ((p: (A,B)) =>
     p match
       {
         case (a1, a2) => f(a1, a2)
       })

  def fOpen3[A,B,C,D](f: ((A, B, C) => D)):
      (((A,B,C)) => D) =
    ((p: (A,B,C)) =>
     p match
       {
         case (a1, a2, a3) => f(a1, a2, a3)
       })

  def contains[A](iter: Iterable[A], x: A) =
    iter.exists((y) => x == y)

  def isSomething[A](x: Option[A]) = x match
    {
      case Some(_) => true
      case None => false
    }

////  def findFirst[A](iter: Iterable[A], pred: A => Boolean): Option[A] =
////    {
////      iter.foldLeft(None: Option[A])
////              ((found: Option[A], next: A) =>
////               found match
////                 {
////                   case Some(x) => x
////                   case None => if (pred(next)) Some(next) else None
////                 })
////    }

}

