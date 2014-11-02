import scala.collection.immutable.{ListMap, Map}
import java.lang.IllegalArgumentException

class MultiSet[A](val u: ListMap[A, Int])
    extends IntMap[A](u)
{
  def this() = this(new ListMap[A, Int]())

  def this(kv: (A,Int)) = this(ListMap[A, Int](kv))

  def addInt(k: A, v: Int): MultiSet[A] =
    if (v > 0)
      new MultiSet(underlying + (k -> v))
    else if (v < 0)
      throw new IllegalArgumentException("Adding entry with negative integer " +
                                         (k,v) + " to " + this)
    else if (isDefinedAt(k))
      // Setting to 0 requires erasing existing mapping
      new MultiSet(underlying - k)
    else this // Not present, leave alone

  def numberOf(x: A): Int = this.getOrElse(x, 0)

  override def get(k: A) = Some(underlying.getOrElse(k, 0))

  override def +[B >: Int](kv: (A, B)) =
    kv match
      {
        case (key, v: Int) =>
          (if (v == 0)
             super.-(key)
           else super.+(kv))
        case _ => super.+(kv)
      }

  override def entryPlus(k: A, n: Int): MultiSet[A] =
    super.entryPlus(k, n).asInstanceOf[MultiSet[A]]

  override def tail:MultiSet[A] = super.tail.asInstanceOf[MultiSet[A]]

  override def toString(): String =
    "MultiSet{" + this.map{ case (k, i: Int) =>
                              if (i > 0)
                                i.toString + "*" + k.toString 
                              else k.toString
                          }.
                    mkString(", ") + 
            "}"
}
// -- This really ought to be augmented with union, intersection, and
// such, if those ops prove useful.


object MultiSet
{

  def fromMap[A](underlying: ListMap[A, Int]) =
    new MultiSet(underlying.filter
                  (kv =>
                    if (kv._2 < 0)
                      throw new IllegalArgumentException
                                     ("Creating MultiSet with negative number "
                                      + kv._2 + " of instances of " + kv._1)
                    else kv._2 > 0))
}