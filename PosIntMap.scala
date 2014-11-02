import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ListMap, Map, MapLike}
import scala.collection.mutable.{Builder, MapBuilder}
import java.lang.IllegalArgumentException


/** Small ListMap whose range is nonnegative integers.  Anything with value
  *  0 is deleted, and any absent entry gets value 0.
  */
class PosIntMap[A] private[PosIntMap] (val underlying: ListMap[A, Int])
    extends Map[A, Int] with MapLike[A, Int, PosIntMap[A]]
{
  import PosIntMap._

  override def newBuilder: Builder[(A, Int), PosIntMap[A]] =
    PosIntMap.newBuilder 

  def numberOf(k: A) = underlying.getOrElse(k, 0)

  override def +[B >: Int](kv: (A, B)) =
   kv match
     {
       case (k, v) =>
         v match
           {
             case i: Int =>
               addInt(k, i)
             case _ =>
               underlying + kv
           }
     }

  def addInt(k: A, v: Int): PosIntMap[A] =
    if (v > 0)
      new PosIntMap(underlying + (k -> v))
    else if (v < 0)
      throw new IllegalArgumentException("Adding entry with negative integer " +
                                         (k,v) + " to " + this)
    else if (underlying.isDefinedAt(k))
      // Setting to 0 requires erasing existing mapping
      new PosIntMap(underlying - k)
    else this

  def entryPlus(k: A, n: Int): PosIntMap[A] = this.addInt(k, numberOf(k) + n)

  override def -(k: A): PosIntMap[A] = new PosIntMap(underlying - k)

  override def get(k: A) = Some(underlying.getOrElse(k, 0))

  override def iterator = underlying.iterator

  override def empty: PosIntMap[A] =
    new PosIntMap(new ListMap[A,Int]())   //// :ListMap[A, Nothing]

  override def toString = "PosInt" + underlying.toString
}

object PosIntMap
{
  def apply[A](entries: (A, Int)*) =  newPosIntMap(ListMap(entries: _*))

  // Factory method with confusing name that ensures private
  // constructor is called only with well-behaved argument --
  def newPosIntMap[A](intMap: ListMap[A,Int]):PosIntMap[A] =
    {
      if (intMap.exists { case (k, i) => (i < 0) })
        {
          throw (new IllegalArgumentException
                                   ("ListMap contains negative values, cannot be converted to"
                                    + "\n  PosIntMap: " + intMap))
        }
      else if (intMap.exists { case (k, i) => (i == 0) })
        new PosIntMap(intMap filter { case (k, i) => (i > 0) })
      else
        new PosIntMap(intMap)
    }

  def newBuilder[A]: Builder[(A,Int), PosIntMap[A]] =
    new Builder[(A, Int), PosIntMap[A]]
     {
       var underConstruction: ListMap[A, Int] = new ListMap[A,Int]()

       def clear():Unit =
       {
         underConstruction = new ListMap[A,Int]()
       }

       def += (v: (A,Int)) =   //// : Builder[(A, Int), PosIntMap[A]] =
       {
         v match
         {
           case (k,i) =>
             if (i>0)
               underConstruction += v
             else if (i<0)
               throw new IllegalArgumentException
                                 ("Adding negative entry (" + k + " -> " +
                                  i + ") to PosIntMap")
         }
         this
       }

       def result: PosIntMap[A] = new PosIntMap(underConstruction)
     } 

  implicit def canBuildFrom[A]:
                CanBuildFrom[Map[A, Int], (A, Int), PosIntMap[A]] =
      new CanBuildFrom[Map[A, Int], (A, Int), PosIntMap[A]] {
        def apply(): Builder[(A,Int), PosIntMap[A]] = newBuilder
        def apply(from: Map[A, Int]): Builder[(A,Int), PosIntMap[A]] = newBuilder
      }

}
