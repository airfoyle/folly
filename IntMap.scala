import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{ListMap, Map, MapLike}
import scala.collection.mutable.{Builder, MapBuilder}


/** Small ListMap whose range is integers.
  */
class IntMap[A] protected(val underlying: ListMap[A, Int])
    extends Map[A, Int] with MapLike[A, Int, IntMap[A]]
{
  import IntMap._

  override def newBuilder: Builder[(A, Int), IntMap[A]] =
    IntMap.newBuilder 

  override def +[B >: Int](kv: (A, B)) =
   kv match
     {
       case (k, v) =>
         v match
           {
             case i: Int =>
               new IntMap(underlying + (k -> i))
             case _ =>
               underlying + kv
           }
     }

  def entryPlus(k: A, n: Int): IntMap[A] =
    (this + (get(k) match
                   {
                     case Some(i: Int) =>
                       (k -> (i + n))
                     case _ => (k -> n)
                   })).
      asInstanceOf[IntMap[A]]

  override def -(k: A): IntMap[A] = new IntMap(underlying - k)

  override def get(k: A) = underlying.get(k)

  override def iterator = underlying.iterator

  override def empty: IntMap[A] =
    new IntMap(new ListMap[A,Int]())   //// :ListMap[A, Nothing]

//  This is unnecessary given all the machinery we've already
// installed --
////  override def tail: IntMap[A] = super.tail.asInstanceOf[IntMap[A]]

  override def toString = "Int" + underlying.toString
}

object IntMap
{
  def apply[A](entries: (A, Int)*) =  new IntMap(ListMap(entries: _*))

  def newBuilder[A]: Builder[(A,Int), IntMap[A]] =
    new Builder[(A, Int), IntMap[A]]
     {
       var underConstruction: ListMap[A, Int] = new ListMap[A,Int]()

       def clear():Unit =
       {
         underConstruction = new ListMap[A,Int]()
       }

       def += (v: (A,Int)) =   //// : Builder[(A, Int), IntMap[A]] =
       {
         underConstruction += v;
         this
       }

       def result: IntMap[A] = new IntMap(underConstruction)
     } 

  implicit def canBuildFrom[A]:
                CanBuildFrom[Map[A, Int], (A, Int), IntMap[A]] =
      new CanBuildFrom[Map[A, Int], (A, Int), IntMap[A]] {
        def apply(): Builder[(A,Int), IntMap[A]] = newBuilder
        def apply(from: Map[A, Int]): Builder[(A,Int), IntMap[A]] = newBuilder
      }

}
